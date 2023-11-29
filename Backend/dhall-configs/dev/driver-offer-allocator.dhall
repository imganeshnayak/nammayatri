let common = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let transporter = ./dynamic-offer-driver-app.dhall

let priority =
      { getCriticalPriorityAPIList =
        [ "/ui/"
        , "/ui/auth/"
        , "/ui/auth/:authId/verify/"
        , "/ui/auth/otp/:authId/resend/"
        , "/ui/auth/logout/"
        , "/ui/driver/profile/"
        , "/ui/driver/location"
        , "/ui/driver/location/:rideId"
        , "/ui/driver/setActivity/"
        , "/ui/driver/searchRequest/quote/respond/"
        , "/internal/drivers/nearby/"
        , "/internal/drivers/location/"
        , "/internal/ride/rideDetails/"
        , "/internal/ride/:rideId/start/"
        , "/internal/ride/:rideId/end/"
        , "/beckn/:merchantId/select/"
        , "/beckn/:merchantId/init/"
        , "/beckn/:merchantId/confirm/"
        , "/beckn/:merchantId/cancel/"
        , "/beckn/:merchantId/status/"
        , "/ui/pickup/route/"
        , "/ui/driver/ride/:rideId/cancel/"
        , "/ui/driver/otpRide/start/"
        , "/ui/driver/ride/:rideId/start/"
        , "/ui/driver/ride/:rideId/end/"
        , "/ui/driver/ride/:rideId/call/customer/"
        , "/ui/exotel/call/customer/number/"
        , "/ui/driver/cleardues/"
        , "/ui/payment/:invoiceId/createOrder/"
        , "/:merchantId/service/juspay/payment/"
        , "/ui/payment/:orderId/status/"
        , "/ui/driver/v2/payments/history/:invoiceId/entity/"
        ]
      , getNonCriticalPriorityAPIList =
        [ "/ui/driver/register/dl/"
        , "/ui/driver/register/validateImage/"
        , "/ui/driver/register/rc/"
        , "/service/idfy/verification/"
        , "/ui/driver/register/verifyAadhaarOtp/"
        , "/ui/driver/register/generateAadhaarOtp/"
        , "/ui/driver/register/unVerifiedAadhaarData/"
        , "/ui/driver/register/status/"
        , "/ui/issue/category/"
        , "/ui/issue/list/"
        , "/ui/issue/option/"
        , "/ui/issue/upload/"
        , "/ui/issue/media/"
        , "/ui/issue/:issueId/delete/"
        , "/ui/plan/:planId/list/"
        , "/ui/plan/:planId/select/"
        , "/ui/plan/:planId/subscribe/"
        , "/ui/plan/suspend/"
        , "/ui/plan/resume/"
        , "/ui/driver/ride/list/"
        , "/ui/driver/profile/"
        , "/ui/driver/profile/stats/"
        , "/ui/driver/profile/summary/"
        , "/ui/rc/all/"
        , "/ui/rc/setStatus/"
        , "/ui/rc/delete/"
        , "/ui/driver/alternateNumber/verify/"
        , "/ui/driver/alternateNumber/validate/"
        , "/ui/driver/alternateNumber/resendOTP/"
        , "/ui/driver/alternateNumber/remove/"
        , "/ui/message/media/"
        , "/ui/message/:messageId/seen/"
        , "/ui/message/:messageId/like/"
        , "/ui/message/:messageId/response/"
        , "/ui/driver/generateReferralCode/"
        , "/ui/driver/referral/"
        , "/ui/driver/leaderBoard/daily/"
        , "/ui/driver/leaderBoard/weekly/"
        ]
      }

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = True
              , logFilePath = "/tmp/driver-offer-scheduler.log"
              , prettyPrinting = True
              }
      , esqDBCfg = appCfg.esqDBCfg
      , metricsPort = +8056
      , hedisCfg = appCfg.hedisCfg
      , hedisClusterCfg = appCfg.hedisClusterCfg
      , hedisNonCriticalCfg = appCfg.hedisCfg
      , hedisNonCriticalClusterCfg = appCfg.hedisClusterCfg
      , hedisMigrationStage = True
      , cutOffHedisCluster = False
      , hedisPrefix = "driver-offer-scheduler"
      , port = +8055
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      , enableRedisLatencyLogging = False
      , enablePrometheusMetricLogging = True
      , groupName = "myGroup"
      , schedulerType = common.schedulerType.RedisBased
      , schedulerSetName = "Scheduled_Jobs"
      , streamName = "Available_Jobs"
      , maxThreads = +10
      , block = +10000
      , readCount = +1
      , priority
      }

in  { appCfg =
            appCfg
        //  { loggerConfig =
                    appCfg.loggerConfig
                //  { logFilePath = "/tmp/driver-offer-allocator.log" }
            }
    , schedulerConfig
    }
