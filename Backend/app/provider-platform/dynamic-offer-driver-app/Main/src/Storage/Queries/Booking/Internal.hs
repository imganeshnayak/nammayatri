{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking.Internal where

import qualified Domain.Types.Booking as Booking
import Domain.Types.DriverQuote (DriverQuote)
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Booking as BeamB
import Storage.Queries.Instances.Person ()

getBookingInfoExceptRentals ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverQuote] ->
  m [Booking.Booking]
getBookingInfoExceptRentals driverQuote =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamB.quoteId $ Se.In personsKeys,
          Se.Is BeamB.status $ Se.Eq Booking.TRIP_ASSIGNED,
          Se.Is BeamB.bookingType $ Se.Not (Se.Eq Booking.RentalBooking)
        ]
    ]
  where
    personsKeys = fetchDriverIDsTextFromQuote driverQuote

fetchDriverIDsTextFromQuote :: [DriverQuote] -> [Text]
fetchDriverIDsTextFromQuote = map (.driverId.getId)
