{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Tracking
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Organization as OQ
import Types.Error
import Types.ProductInfo as ProductInfo
import Utils.Common (validateContext)
import qualified Utils.Notifications as Notify

track :: Person.Person -> TrackTripReq -> FlowHandler TrackTripRes
track person req = withFlowHandlerBecknAPI $ do
  let prodInstId = req ^. #message . #order_id
  prodInst <- MPI.findById $ Id prodInstId
  case_ <- MC.findIdByPerson person (prodInst ^. #_caseId)
  msgId <- L.generateGUID
  let txnId = getId $ case_ ^. #_id
  let context = req ^. #context & #_transaction_id .~ txnId & #_message_id .~ msgId
  organization <-
    OQ.findOrganizationById (Id $ prodInst ^. #_organizationId)
      >>= fromMaybeM OrgNotFound
  (info :: ProductInfo) <-
    (decodeFromText =<< (prodInst ^. #_info))
      & fromMaybeM (PIFieldNotPresent "info")
  tracker <- info ^. #_tracker & fromMaybeM (InternalError "PI.info has no tracker field")
  let gTripId = tracker ^. #_trip . #id
  gatewayUrl <- organization ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  AckResponse {} <- Gateway.track gatewayUrl $ req & #context .~ context & ((#message . #order_id) .~ gTripId)
  return $ AckResponse context (ack ACK) Nothing

trackCb :: Organization.Organization -> OnTrackTripReq -> FlowHandler OnTrackTripRes
trackCb _org req = withFlowHandlerBecknAPI $ do
  validateContext "on_track" $ req ^. #context
  let context = req ^. #context
  case req ^. #contents of
    Right msg -> do
      let tracking = msg ^. #tracking
          caseId = Id $ context ^. #_transaction_id
      case_ <- MC.findById caseId
      prodInst <- MPI.listAllProductInstance (ProductInstance.ByApplicationId caseId) [ProductInstance.CONFIRMED]
      let confirmedProducts = prodInst
      res <-
        case length confirmedProducts of
          0 -> return $ Right ()
          1 -> do
            let productInst = head confirmedProducts
                personId = Case._requestor case_
            orderPi <- MPI.findByParentIdType (productInst ^. #_id) Case.RIDEORDER
            mtracker <- updateTracker orderPi tracking
            whenJust mtracker (\t -> Notify.notifyOnTrackCb personId t case_)
            return $ Right ()
          _ -> return $ Left "Multiple products confirmed, ambiguous selection"
      res & fromEitherM InvalidRequest
      return $ AckResponse context (ack ACK) Nothing
    Left err -> do
      logTagError "on_track_trip req" $ "on_track_trip error: " <> show err
      return $ AckResponse context (ack ACK) Nothing

updateTracker :: ProductInstance.ProductInstance -> Maybe Tracking -> Flow (Maybe Tracker)
updateTracker prodInst mtracking = do
  let minfo = decodeFromText =<< prodInst ^. #_info
  case minfo of
    Nothing -> return Nothing
    Just info -> do
      let mtracker = updTracker info mtracking
          uInfo = info {ProductInfo._tracker = mtracker}
          updatedPrd = prodInst {ProductInstance._info = Just $ encodeToText uInfo}
      MPI.updateMultiple (prodInst ^. #_id) updatedPrd
      return mtracker
  where
    updTracker info tracking =
      case info ^. #_tracker of
        Just tracker -> Just (Tracker (tracker ^. #_trip) $ fromBeckn <$> tracking)
        Nothing -> Nothing
