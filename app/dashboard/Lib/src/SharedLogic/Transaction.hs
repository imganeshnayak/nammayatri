module SharedLogic.Transaction
  ( emptyRequest,
    emptyResponse,
    buildTransaction,
    withTransactionStoring,
    withResponseTransactionStoring,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified Data.Text as Text
import qualified Domain.Types.Transaction as DT
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (encodeToText, throwError)
import qualified Storage.Queries.Transaction as QT
import Tools.Auth
import qualified Tools.Error as E

emptyRequest :: Maybe ()
emptyRequest = Nothing

emptyResponse :: Maybe ()
emptyResponse = Nothing

-- we need to validate id length to avoid internal error while storing transaction
validateId :: (Log m, MonadThrow m) => Maybe (Id domain) -> Text -> m ()
validateId Nothing _ = pure ()
validateId (Just someId) domainName =
  unless (Text.length someId.getId == 36) $
    throwError . InvalidRequest $ domainName <> "Id should have length 36"

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  DT.Endpoint ->
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe (Id Common.Ride) ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo commonDriverId commonRideId request = do
  uid <- generateGUID
  now <- getCurrentTime
  validateId commonDriverId "driver"
  validateId commonRideId "ride"
  pure
    DT.Transaction
      { id = uid,
        requestorId = apiTokenInfo.personId,
        merchantId = Just apiTokenInfo.merchant.id,
        request = encodeToText . Common.hideSecrets <$> request,
        response = Nothing,
        responseError = Nothing,
        createdAt = now,
        ..
      }

-- | Run client call and store transaction to DB.
--
-- If client call fails, then write error code to transaction.
withTransactionStoring ::
  ( Esq.EsqDBFlow m r,
    MonadCatch m
  ) =>
  DT.Transaction ->
  m response ->
  m response
withTransactionStoring =
  withResponseTransactionStoring' (const emptyResponse)

-- | Run client call and store transaction to DB.
--
-- If client call successed, then write response to transaction, with secrets hiding
-- Else write error code to transaction.
withResponseTransactionStoring ::
  ( Esq.EsqDBFlow m r,
    MonadCatch m,
    Common.HideSecrets response
  ) =>
  DT.Transaction ->
  m response ->
  m response
withResponseTransactionStoring =
  withResponseTransactionStoring' (Just . Common.hideSecrets)

withResponseTransactionStoring' ::
  ( Esq.EsqDBFlow m r,
    MonadCatch m,
    ToJSON transactionResponse
  ) =>
  (response -> Maybe transactionResponse) ->
  DT.Transaction ->
  m response ->
  m response
withResponseTransactionStoring' responseModifier transaction clientCall = handle errorHandler $ do
  response <- clientCall
  Esq.runTransaction $
    QT.create $ transaction{response = encodeToText <$> responseModifier response}
  pure response
  where
    -- This code do not handle ExternalAPICallError, only E.Error
    errorHandler (err :: E.Error) = do
      Esq.runTransaction $
        QT.create transaction{responseError = Just $ show err}
      throwError err
