module Beckn.Constants.APIErrorCode where

import Beckn.Types.API.Common

internalServerErr :: ErrorResponse
internalServerErr =
  ErrorResponse
    { status = "FAILURE"
    , responseCode = "INTERNAL_SERVER_ERROR"
    , responseMessage = "INTERNAL_SERVER_ERROR"
    }
