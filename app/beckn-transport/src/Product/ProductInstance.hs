{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Models.Case as CQ
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Allocation as AQ
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DSQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import Types.App
import Types.Error
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: SR.RegistrationToken -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list SR.RegistrationToken {..} status csTypes limitM offsetM = withFlowHandler $ do
  person <- PersQ.findPersonById (Id _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      result <- PIQ.productInstanceJoin limit offset csTypes orgId status
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> (_case <$> result)) (Case._toLocationId <$> (_case <$> result))
      return $ buildResponse locList <$> result
    Nothing ->
      throwError PersonOrgIdNotPresent
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: [Loc.Location] -> ProductInstanceRes -> ProductInstanceRes
    buildResponse locList res =
      ProductInstanceRes
        { _case = res ^. #_case,
          _product = res ^. #_product,
          _productInstance = res ^. #_productInstance,
          _fromLocation = find (\x -> Case._fromLocationId (res ^. #_case) == getId (Loc._id x)) locList,
          _toLocation = find (\x -> Case._toLocationId (res ^. #_case) == getId (Loc._id x)) locList
        }

update :: SR.RegistrationToken -> Id PI.ProductInstance -> ProdInstUpdateReq -> FlowHandler ProdInstInfo
update SR.RegistrationToken {..} piId req = withFlowHandler $ do
  requestor <- PersQ.findPersonById (Id _EntityId)
  ordPi <- PIQ.findById piId
  searchPi <-
    ordPi ^. #_parentId & fromMaybeM PIParentIdNotPresent
      >>= PIQ.findById
  PI.validateStatusTransition (ordPi ^. #_status) newStatus
    & either (throwErrorWithInfo InvalidRequest) pure
  validateRequest ordPi req requestor
  let requestedByDriver = requestor ^. #_role == SP.DRIVER
  updateTrip (searchPi ^. #_id) newStatus requestedByDriver
  notifyUpdateToBAP searchPi ordPi newStatus
  PIQ.findById piId
  where
    newStatus = req ^. #_status

notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> PI.ProductInstanceStatus -> Flow ()
notifyUpdateToBAP searchPi orderPi updatedStatus = do
  -- Send callback to BAP
  bapOrg <- fetchBapOrganization $ orderPi ^. #_caseId
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM OrgCallbackUrlNotSet
  notifyTripDetailsToGateway searchPi orderPi callbackUrl
  notifyStatusUpdateReq searchPi updatedStatus callbackUrl
  where
    fetchBapOrganization caseId = do
      prodCase <- fetchCase caseId >>= fromMaybeM CaseNotFound
      bapOrgId <- prodCase ^. #_udf4 & fromMaybeM CaseBapOrgIdNotPresent
      OQ.findOrganizationById $ Id bapOrgId
    fetchCase caseId = do
      prodCase <- QCase.findById caseId
      checkDBError prodCase

listDriverRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listDriverRides SR.RegistrationToken {..} personId = withFlowHandler $ do
  user <- PersQ.findPersonById (Id _EntityId)
  person <- PersQ.findPersonById (Id personId)
  hasAccess user person
  rideList <- PIQ.findAllByPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (PI._fromLocation <$> rideList)) (catMaybes (PI._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ throwError Unauthorized
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (getId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (getId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listVehicleRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listVehicleRides SR.RegistrationToken {..} vehicleId = withFlowHandler $ do
  user <- PersQ.findPersonById (Id _EntityId)
  vehicle <- VQ.findVehicleById (Id vehicleId)
  hasAccess user vehicle
  rideList <- PIQ.findAllByVehicleId (Just vehicleId)
  locList <- LQ.findAllByLocIds (catMaybes (PI._fromLocation <$> rideList)) (catMaybes (PI._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user vehicle =
      when
        ( isNothing (SP._organizationId user)
            || (SP._organizationId user /= (V._organizationId <$> vehicle))
        )
        $ throwError Unauthorized
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (getId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (getId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Text -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} piId csType = withFlowHandler $ do
  prodInst <- PIQ.findById (Id piId)
  piList <-
    prodInst ^. #_parentId & fromMaybeM PIParentIdNotPresent
      >>= PIQ.findAllByParentId
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (PI._caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (PI._caseId <$> piList)
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      find (\x -> Case._fromLocationId cs == getId (Loc._id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case._toLocationId cs == getId (Loc._id x)) locList
        prepare pcs from to =
          APICase.CaseRes
            { _case = pcs,
              _fromLocation = from,
              _toLocation = to
            }

-- Core Utility methods are below

assignDriver :: Id PI.ProductInstance -> Id Driver -> Flow ()
assignDriver productInstanceId driverId = do
  ordPi <- PIQ.findById productInstanceId
  searchPi <- PIQ.findById =<< fromMaybeM PIParentIdNotPresent (ordPi ^. #_parentId)
  piList <-
    ordPi ^. #_parentId & fromMaybeM PIParentIdNotPresent
      >>= PIQ.findAllByParentId
  headPi <- case piList of
    p : _ -> pure p
    [] -> throwError PIDoesNotExist
  driver <- PersQ.findPersonById $ cast driverId
  vehicleId <-
    driver ^. #_udf1
      & fromMaybeMWithInfo PersonFieldNotPresent "_udf1 is null. Vehicle is not set."
      <&> Id
  vehicle <-
    VQ.findVehicleById vehicleId
      >>= fromMaybeM VehicleNotFound
  let piIdList = PI._id <$> piList

  DB.runSqlDBTransaction (AQ.assignDriver productInstanceId piIdList vehicle driver)

  notifyUpdateToBAP searchPi ordPi PI.TRIP_ASSIGNED
  Notify.notifyDriver notificationType notificationTitle (message headPi) driver
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message p' =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst (PI._startTime p') <> ".",
          "Check the app for more details."
        ]

validateRequest :: PI.ProductInstance -> ProdInstUpdateReq -> SP.Person -> Flow ()
validateRequest ride req requestor = do
  when (requestor ^. #_role == SP.DRIVER) checkIfDriverBelongsToRide
  case (ride ^. #_status, newStatus, requestor ^. #_role) of
    (PI.TRIP_ASSIGNED, PI.CANCELLED, SP.DRIVER) -> ok
    (PI.TRIP_ASSIGNED, PI.CANCELLED, SP.ADMIN) -> ok
    (PI.TRIP_ASSIGNED, PI.INPROGRESS, SP.DRIVER) -> do
      inAppOtpCode <- ride ^. #_udf4 & fromMaybeM PIOTPNotPresent
      tripOtpCode <- req ^. #_otpCode & fromMaybeMWithInfo InvalidRequest "You should pass OTP."
      unless (inAppOtpCode == tripOtpCode) $ throwError IncorrectOTP
    (PI.INPROGRESS, PI.COMPLETED, SP.DRIVER) ->
      DSQ.updateIdleTimeFlow . cast $ requestor ^. #_id
    (PI.INPROGRESS, PI.COMPLETED, SP.ADMIN) -> ok
    (oldStatus', newStatus', who) -> do
      logError "Invalid update operation" . T.pack $
        "From " <> show oldStatus'
          <> " to "
          <> show newStatus'
          <> " by "
          <> show who
      throwErrorWithInfo InvalidRequest "Invalid update operation."
  where
    newStatus = req ^. #_status
    ok = pure ()
    checkIfDriverBelongsToRide = do
      rideDriver <- ride ^. #_personId & fromMaybeM Unauthorized
      unless (requestor ^. #_id == rideDriver) $ throwError Unauthorized

notifyTripDetailsToGateway :: PI.ProductInstance -> PI.ProductInstance -> BaseUrl -> Flow ()
notifyTripDetailsToGateway searchPi orderPi callbackUrl = do
  trackerCase <- CQ.findByParentCaseIdAndType (searchPi ^. #_caseId) Case.LOCATIONTRACKER
  transporter <- OQ.findOrganizationById . Id $ searchPi ^. #_organizationId
  let bppShortId = getShortId $ transporter ^. #_shortId
  parentCase <- CQ.findById (searchPi ^. #_caseId)
  case (trackerCase, parentCase) of
    (Just x, y) -> BP.notifyTripInfoToGateway orderPi x y callbackUrl bppShortId
    _ -> return ()

updateTrip :: Id PI.ProductInstance -> PI.ProductInstanceStatus -> Bool -> Flow ()
updateTrip searchPiId newStatus requestedByDriver = do
  piList <- PIQ.findAllByParentId searchPiId
  trackerCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.RIDEORDER
  case newStatus of
    PI.CANCELLED -> do
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      BP.cancelRide (cast $ orderPi ^. #_id) requestedByDriver
    PI.INPROGRESS -> do
      _ <- PIQ.updateStatusByIdsFlow (PI._id <$> piList) newStatus
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      CQ.updateStatus (Case._id orderCase_) Case.INPROGRESS
      return ()
    PI.COMPLETED -> do
      _ <- PIQ.updateStatusByIdsFlow (PI._id <$> piList) newStatus
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id orderCase_) Case.COMPLETED
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      updateOnRide (cast <$> PI._personId orderPi) False
      whenJust (orderPi ^. #_personId) (DSQ.updateIdleTimeFlow . cast)
      return ()
    _ -> return ()
  where
    updateOnRide Nothing _ = pure ()
    updateOnRide (Just personId) status = DriverInformation.updateOnRideFlow personId status

notifyStatusUpdateReq :: PI.ProductInstance -> PI.ProductInstanceStatus -> BaseUrl -> Flow ()
notifyStatusUpdateReq searchPi status callbackUrl = do
  transporterOrg <- findOrganization
  let bppShortId = getShortId $ transporterOrg ^. #_shortId
  case status of
    PI.CANCELLED -> do
      admins <- getAdmins transporterOrg
      BP.notifyCancelToGateway (getId $ searchPi ^. #_id) callbackUrl bppShortId
      Notify.notifyCancelReqByBP searchPi admins
    PI.TRIP_REASSIGNMENT -> do
      admins <- getAdmins transporterOrg
      Notify.notifyDriverCancelledRideRequest searchPi admins
      notifyStatusToGateway bppShortId
    _ -> notifyStatusToGateway bppShortId
  where
    findOrganization = OQ.findOrganizationById $ Id $ searchPi ^. #_organizationId
    getAdmins transporterOrg = do
      if transporterOrg ^. #_enabled
        then PersQ.findAllByOrgIds [SP.ADMIN] [PI._organizationId searchPi]
        else pure []
    notifyStatusToGateway bppShortId = do
      trackerPi <- PIQ.findByParentIdType (Just $ searchPi ^. #_id) Case.LOCATIONTRACKER
      BP.notifyServiceStatusToGateway (getId $ searchPi ^. #_id) trackerPi callbackUrl bppShortId
