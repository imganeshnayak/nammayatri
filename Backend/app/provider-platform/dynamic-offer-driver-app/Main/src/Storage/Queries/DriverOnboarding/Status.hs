{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromRight" #-}

module Storage.Queries.DriverOnboarding.Status where

import Data.List (zip7)
import qualified Database.Beam as B
import Domain.Types.DriverInformation
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate (VehicleRegistrationCertificate)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person
import qualified EulerHS.Language as L
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (Log)
import Kernel.Types.Id
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverOnboarding.DriverLicense as BeamDL
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamRC
import qualified Storage.Beam.DriverOnboarding.IdfyVerification as BeamIV
import qualified Storage.Beam.DriverOnboarding.Image as BeamI
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Queries.DriverOnboarding.DriverLicense ()
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation ()
import qualified Storage.Queries.DriverOnboarding.IdfyVerification ()
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()
import qualified Storage.Queries.Person ()

data DriverDocsInfo = DriverDocsInfo
  { person :: Person,
    license :: Maybe DriverLicense,
    licenseVerificationReq :: Maybe IV.IdfyVerification,
    assocReg :: Maybe (DriverRCAssociation, VehicleRegistrationCertificate),
    regVerificationReq :: Maybe IV.IdfyVerification,
    driverInfo :: DriverInformation,
    numLicenseImages :: Int,
    numVehRegImages :: Int
  }

-- imagesAggTableCTEbyDoctype :: Image.ImageType -> SqlQuery (From (SqlExpr (Value PersonTId), SqlExpr (Value Int)))
-- imagesAggTableCTEbyDoctype imageType = with $ do
--   image <- from $ table @ImageT
--   where_ $ image ^. ImageImageType ==. val imageType
--   groupBy $ image ^. ImagePersonId
--   pure (image ^. ImagePersonId, count @Int $ image ^. ImageId)

-- imagesAggTableCTEbyDoctype' = do

imagesAggTableCTEbyDoctype :: L.MonadFlow m => Image.ImageType -> m [(Text, Int)]
imagesAggTableCTEbyDoctype imageType' = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\image' -> (B.group_ (BeamI.personId image'), B.as_ @Int B.countAll_)) $
            B.filter_' (\(BeamI.ImageT {..}) -> imageType B.==?. B.val_ imageType') $
              B.all_ (BeamCommon.image BeamCommon.atlasDB)
  pure (Prelude.fromRight [] resp)

-- baseDriverDocumentsInfoQuery ::
--   From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
--   From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
--   From
--     ( Table PersonT
--         :& MbTable DriverLicenseT
--         :& MbTable IdfyVerificationT
--         :& MbTable DriverRCAssociationT
--         :& MbTable VehicleRegistrationCertificateT
--         :& MbTable IdfyVerificationT
--         :& Table DriverInformationT
--         :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
--         :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
--     )
-- baseDriverDocumentsInfoQuery licenseImagesAggTable vehicleRegistrationImagesAggTable =
--   table @PersonT
--     `Esq.leftJoin` table @DriverLicenseT `Esq.on` (\(p :& l) -> just (p ^. PersonTId) ==. l ?. DriverLicenseDriverId)
--     `Esq.leftJoin` table @IdfyVerificationT
--       `Esq.on` ( \(p :& _ :& licReq) ->
--                    just (p ^. PersonTId) ==. licReq ?. IdfyVerificationDriverId
--                      &&. licReq ?. IdfyVerificationDocType ==. just (val Image.DriverLicense)
--                )
--     `Esq.leftJoin` table @DriverRCAssociationT
--       `Esq.on` (\(p :& _ :& _ :& rcAssoc) -> just (p ^. PersonTId) ==. rcAssoc ?. DriverRCAssociationDriverId)
--     `Esq.leftJoin` table @VehicleRegistrationCertificateT
--       `Esq.on` (\(_ :& _ :& _ :& rcAssoc :& regCert) -> rcAssoc ?. DriverRCAssociationRcId ==. regCert ?. VehicleRegistrationCertificateTId)
--     `Esq.leftJoin` table @IdfyVerificationT
--       `Esq.on` ( \(p :& _ :& _ :& _ :& _ :& regReq) ->
--                    just (p ^. PersonTId) ==. regReq ?. IdfyVerificationDriverId
--                      &&. regReq ?. IdfyVerificationDocType ==. just (val Image.VehicleRegistrationCertificate)
--                )
--     `Esq.innerJoin` table @DriverInformationT
--       `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& driverInfo) -> p ^. PersonTId ==. driverInfo ^. DriverInformationDriverId)
--     `Esq.leftJoin` licenseImagesAggTable
--       `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& _ :& (licImgPersonId, _)) -> just (p ^. PersonTId) ==. licImgPersonId)
--     `Esq.leftJoin` vehicleRegistrationImagesAggTable
--       `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& _ :& _ :& (vehRegImgPersonId, _)) -> just (p ^. PersonTId) ==. vehRegImgPersonId)

fetchDriverDocsInfo :: (L.MonadFlow m, Log m) => Id Merchant -> Maybe (NonEmpty (Id Driver)) -> m [DriverDocsInfo]
fetchDriverDocsInfo merchantId' mbDriverIds = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        do
          person' <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
          dl' <- B.leftJoin_' (B.all_ $ BeamCommon.driverLicense BeamCommon.atlasDB) (\dl'' -> BeamDL.driverId dl'' B.==?. BeamP.id person')
          idfy' <- B.leftJoin_' (B.all_ $ BeamCommon.idfyVerification BeamCommon.atlasDB) (\idfy'' -> BeamIV.driverId idfy'' B.==?. BeamP.id person')
          drc' <- B.leftJoin_' (B.all_ $ BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\drc'' -> BeamRC.driverId drc'' B.==?. BeamP.id person')
          vc' <- B.leftJoin_' (B.all_ $ BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB) (\vc'' -> BeamRC.rcId drc' B.==?. B.just_ (BeamVRC.id vc''))
          idfy'' <- B.leftJoin_' (B.all_ $ BeamCommon.idfyVerification BeamCommon.atlasDB) (\idfy''' -> BeamIV.driverId idfy''' B.==?. BeamP.id person' B.&&?. BeamIV.docType idfy''' B.==?. B.val_ Image.VehicleRegistrationCertificate)
          di' <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\di'' -> BeamDI.driverId di'' B.==?. BeamP.id person')
          pure (person', dl', idfy', drc', vc', idfy'', di')
  resDom <- case res of
    Right res' -> do
      p <- catMaybes <$> mapM fromTType' (fst' <$> res')
      -- dl <- mapM (maybe (pure Nothing) (fromTType')) dl''
      dl <- mapM (maybe (pure Nothing) fromTType') (snd' <$> res')
      idfy <- mapM (maybe (pure Nothing) fromTType') (thd' <$> res')
      drc <- mapM (maybe (pure Nothing) fromTType') (fth' <$> res')
      vc <- mapM (maybe (pure Nothing) fromTType') (fft' <$> res')
      idfy_ <- mapM (maybe (pure Nothing) fromTType') (six' <$> res')
      di <- catMaybes <$> mapM fromTType' (sev' <$> res')
      pure $ zip7 p dl idfy drc vc idfy_ di
    Left _ -> pure []
  imagesCountLic <- imagesAggTableCTEbyDoctype Image.DriverLicense
  imagesCountVehReg <- imagesAggTableCTEbyDoctype Image.VehicleRegistrationCertificate
  let resAndImageCount = foldl' (joinResAndLic imagesCountLic) [] resDom
      resImageAndVehCount = foldl' (joinResAndVeh imagesCountVehReg) [] resAndImageCount
      driverDocs' = filter (\(p, _, _, _, _, _, _, _, _) -> p.merchantId == merchantId' && maybe True (\dIds -> (getId p.id) `elem` (getId <$> toList dIds)) mbDriverIds) resImageAndVehCount
      driverDocs = map (\(p, dl, idfy, drc, vc, idfy_, di, lic, veh) -> (p, dl, idfy, drc, vc, idfy_, di, snd <$> lic, snd <$> veh)) driverDocs'
  pure $ map mkDriverDocsInfo driverDocs
  where
    joinResAndLic imagesCountLic' resAndImageCount (p, dl, idfy, drc, vc, idfy_, di) =
      let resAndImageCount' = filter (\(id_, _) -> getId p.id == id_) imagesCountLic'
       in resAndImageCount <> if not (null resAndImageCount') then (\(id_, count'') -> (p, dl, idfy, drc, vc, idfy_, di, Just (id_, count''))) <$> resAndImageCount' else [(p, dl, idfy, drc, vc, idfy_, di, Nothing)]
    joinResAndVeh imagesCountVehReg' resImageAndVehCount (p, dl, idfy, drc, vc, idfy_, di, lic) =
      let resImageAndVehCount' = filter (\(id_, _) -> getId p.id == id_) imagesCountVehReg'
       in resImageAndVehCount <> if not (null resImageAndVehCount') then (\(id_, count'') -> (p, dl, idfy, drc, vc, idfy_, di, lic, Just (id_, count''))) <$> resImageAndVehCount' else [(p, dl, idfy, drc, vc, idfy_, di, lic, Nothing)]
    fst' (x, _, _, _, _, _, _) = x
    snd' (_, x, _, _, _, _, _) = x
    thd' (_, _, x, _, _, _, _) = x
    fth' (_, _, _, x, _, _, _) = x
    fft' (_, _, _, _, x, _, _) = x
    six' (_, _, _, _, _, x, _) = x
    sev' (_, _, _, _, _, _, x) = x

-- fetchDriverDocsInfo :: (Transactionable m) => Id Merchant -> Maybe (NonEmpty (Id Driver)) -> m [DriverDocsInfo]
-- fetchDriverDocsInfo merchantId mbDriverIds = fmap (map mkDriverDocsInfo) $
--   Esq.findAll $ do
--     imagesCountLic <- imagesAggTableCTEbyDoctype Image.DriverLicense
--     imagesCountVehReg <- imagesAggTableCTEbyDoctype Image.VehicleRegistrationCertificate
--     person :& license :& licReq :& assoc :& registration :& regReq :& driverInfo :& licImages :& vehRegImages <-
--       from $ baseDriverDocumentsInfoQuery imagesCountLic imagesCountVehReg

--     where_ $
--       maybe (val True) (\ids -> person ^. PersonTId `in_` valList (map (toKey . coerce) $ toList ids)) mbDriverIds
--         &&. person ^. PersonMerchantId ==. (val . toKey $ merchantId)
--     pure (person, license, licReq, assoc, registration, regReq, driverInfo, snd licImages, snd vehRegImages)

mkDriverDocsInfo ::
  ( Person,
    Maybe DriverLicense,
    Maybe IV.IdfyVerification,
    Maybe DriverRCAssociation,
    Maybe VehicleRegistrationCertificate,
    Maybe IV.IdfyVerification,
    DriverInformation,
    Maybe Int,
    Maybe Int
  ) ->
  DriverDocsInfo
mkDriverDocsInfo (p, l, licReq, a, r, regReq, driverInfo, licImages, vehRegImages) =
  DriverDocsInfo p l licReq ((,) <$> a <*> r) regReq driverInfo (def0 licImages) (def0 vehRegImages)
  where
    def0 = fromMaybe 0
