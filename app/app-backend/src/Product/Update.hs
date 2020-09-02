{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Update where

import App.Types
import Beckn.Types.API.Update
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Tracking
import Beckn.Types.Mobility.Trip
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Models.ProductInstance as MPI
import qualified Types.ProductInfo as ProdInfo

onUpdate :: OnUpdateReq -> FlowHandler AckResponse
onUpdate req = withFlowHandler $ do
  -- TODO: Verify api key here
  L.logInfo @Text "on_update req" (show req)
  case req ^. #contents of
    Right msg -> do
      let trip = msg ^. #order . #_trip
          pid = ProductInstanceId $ msg ^. #order . #_id
      orderPi <- MPI.findByParentIdType (Just pid) Case.RIDEORDER
      let mprdInfo = decodeFromText =<< (orderPi ^. #_info)
          uInfo = getUpdatedProdInfo trip mprdInfo $ toBeckn <$> (ProdInfo._tracking =<< ProdInfo._tracker =<< mprdInfo)
          uPrd =
            orderPi
              { SPI._info = encodeToText <$> uInfo
              }
      MPI.updateMultiple (orderPi ^. #_id) uPrd
    Left err -> L.logError @Text "on_update req" $ "on_update error: " <> show err
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    getUpdatedProdInfo :: Maybe Trip -> Maybe ProdInfo.ProductInfo -> Maybe Tracking -> Maybe ProdInfo.ProductInfo
    getUpdatedProdInfo (Just trip) (Just prdInfo) mtracking =
      let utracker = ProdInfo.Tracker {_trip = fromBeckn trip, _tracking = fromBeckn <$> mtracking}
       in Just $ prdInfo {ProdInfo._tracker = Just utracker}
    getUpdatedProdInfo _ _ _ = Nothing
