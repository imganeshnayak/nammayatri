module Storage.Queries.Driveronboarding.OperatingCity where
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Driveronboarding.OperatingCity
import Storage.Tabular.Driveronboarding.OperatingCity
import Domain.Types.Organization
import Beckn.Types.Common (Log, EncFlow)

create :: OperatingCity -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id OperatingCity ->
  m (Maybe OperatingCity)
findById = Esq.findById

findByorgId ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Id Organization ->
  m (Maybe OperatingCity)
findByorgId personid = do
  findOne $ do
    vechileRegCert <- from $ table @OperatingCityT
    where_ $ vechileRegCert ^. OperatingCityOrganizationId ==. val (toKey personid)
    return vechileRegCert


-- findRentalFarePoliciesByOrg ::
--   Transactionable m =>
--   Id Organization ->
--   m [OperatingCity]
-- findRentalFarePoliciesByOrg orgId = do
--   Esq.findOne $ do
--     operatingCity  <- from $ table @OperatingCityT
--     where_ $
--       operatingCity ^. OperatingCityOrganizationId ==. val (toKey orgId)
--     return OperatingCity


