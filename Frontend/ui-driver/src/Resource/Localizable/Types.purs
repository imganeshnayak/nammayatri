{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Language.Types where

data STR = LETS_GET_STARTED
        | LANGUAGE_UPDATED
        | YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION
        | VIEW_STATUS
        | GO_HOME
        | SELECT_LANGUAGE
        | WHICH_LANGUAGE_DO_YOU_PREFER
        | T_C
        | ENTER_MOBILE_NUMBER
        | BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR
        | ENTER_OTP
        | DIDNT_RECIEVE_OTP
        | RESEND_OTP
        | PLEASE_ENTER_VALID_OTP
        | INVALID_MOBILE_NUMBER
        | REGISTER
        | MOBILE_NUMBER
        | AUTO_READING_OTP
        | UPLOAD_DRIVING_LICENSE
        | UPLOAD_BACK_SIDE
        | UPLOAD_FRONT_SIDE
        | BACK_SIDE
        | FRONT_SIDE
        | NEXT
        | LICENSE_INSTRUCTION_PICTURE
        | LICENSE_INSTRUCTION_CLARITY
        | REGISTRATION_STEPS
        | PROGRESS_SAVED
        | DRIVING_LICENSE
        | AADHAR_CARD
        | BANK_DETAILS
        | VEHICLE_DETAILS
        | UPLOAD_FRONT_BACK
        | EARNINGS_WILL_BE_CREDITED
        | FILL_VEHICLE_DETAILS
        | FOLLOW_STEPS
        | REGISTRATION
        | UPLOAD_ADHAAR_CARD
        | ADHAAR_INTRUCTION_PICTURE
        | ADD_VEHICLE_DETAILS
        | VEHICLE_REGISTRATION_NUMBER
        | ENTER_VEHICLE_NO
        | VEHICLE_TYPE
        | VEHICLE_MODEL_NAME
        | ENTER_MODEL_NAME
        | VEHICLE_COLOUR
        | ENTER_VEHICLE_COLOUR
        | UPLOAD_REGISTRATION_CERTIFICATE
        | UPLOAD_RC
        | PREVIEW
        | CHOOSE_VEHICLE_TYPE
        | MAX_IMAGES
        | RE_ENTER_BENIFICIARY_NUMBER
        | IFSC_CODE
        | BENIFICIARY_NUMBER
        | SENDING_OTP
        | LOADING
        | PLEASE_WAIT_WHILE_IN_PROGRESS
        | YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN
        | ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
        | COUNTRY_CODE_INDIA
        | ENTER_OTP_SENT_TO
        | OTP_SENT_TO
        | ENTER_ACCOUNT_NUMBER
        | ADD_BANK_DETAILS
        | ENTER_IFSC_CODE
        | SUBMIT
        | PERSONAL_DETAILS
        | LANGUAGES
        | HELP_AND_FAQ
        | ABOUT
        | LOGOUT
        | UPDATE
        | EDIT
        | DELETE
        | VIEW
        | ISSUE_NO
        | ADD_VOICE_NOTE
        | VOICE_NOTE_ADDED
        | ADDED_IMAGES
        | NO_IMAGES_ADDED
        | ASK_DETAILS_MESSAGE
        | ASK_DETAILS_MESSAGE_REVERSED
        | SELECT_OPTION
        | SELECT_OPTION_REVERSED
        | ISSUE_SUBMITTED_MESSAGE
        | SUBMIT_ISSUE_DETAILS
        | IMAGE_PREVIEW
        | RIDE_REPORT_ISSUE
        | I_DONT_KNOW_WHICH_RIDE
        | REPORT_ISSUE_CHAT_PLACEHOLDER String
        | ADDED_VOICE_NOTE
        | NO_VOICE_NOTE_ADDED
        | CALL_CUSTOMER_TITLE
        | CALL_CUSTOMER_DESCRIPTION
        | PLACE_CALL
        | PLACE_CALL_REQUEST
        | ADD_IMAGE
        | ADD_ANOTHER
        | IMAGES_ADDED
        | ISSUE_SUBMITTED_TEXT
        | CHOOSE_AN_OPTION
        | IMAGE_ADDED
        | DONE
        | RECORD_VOICE_NOTE
        | AUTO
        | NAME
        | PRIVACY_POLICY
        | LOGO
        | ABOUT_APP_DESCRIPTION
        | TERMS_AND_CONDITIONS
        | UPDATE_VEHICLE_DETAILS
        | HELP_AND_SUPPORT
        | NOTE
        | VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS
        | THANK_YOU_FOR_WRTITTING_US
        | GO_TO_HOME
        | YOUR_RECENT_RIDE
        | YOUR_RECENT_TRIP
        | ALL_TOPICS
        | REPORT_AN_ISSUE_WITH_THIS_TRIP
        | YOU_RATED
        | VIEW_ALL_RIDES
        | WRITE_TO_US
        | SUBJECT
        | YOUR_EMAIL_ID
        | MORE_OPTIONS
        | DESCRIBE_YOUR_ISSUE
        | GETTING_STARTED_AND_FAQ
        | ONGOING_ISSUES
        | RESOLVED_ISSUES
        | FOR_OTHER_ISSUES_WRITE_TO_US
        | CALL_SUPPORT_CENTER
        | YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE
        | REGISTRATION_CERTIFICATE_IMAGE
        | HOME
        | RIDES
        | MY_RIDES
        | PROFILE
        | ENTER_DRIVING_LICENSE_NUMBER
        | TRIP_DETAILS
        | BY_CASH
        | ONLINE_
        | GO_ONLINE_POPUP
        | DISTANCE
        | REPORT_AN_ISSUE
        | TIME_TAKEN
        | MAPS
        | CALL
        | START_RIDE
        | CANCEL_RIDE
        | PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
        | MANDATORY
        | END_RIDE
        | RIDE_COMPLETED_WITH
        | COLLECT_AMOUNT_IN_CASH
        | CASH_COLLECTED
        | OFFLINE
        | ACCEPT_FOR
        | DECLINE
        | REQUEST
        | YOU_ARE_OFFLINE
        | YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS
        | GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE
        | CANCEL
        | GO_OFFLINE
        | IS_WAITING_FOR_YOU
        | YOU_ARE_ON_A_RIDE
        | PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP
        | COMPLETED_
        | CANCELLED_
        | WHERE_IS_MY_LICENSE_NUMBER
        | WE_NEED_SOME_ACCESS
        | ALLOW_ACCESS
        | ENTER_RC_NUMBER
        | WHERE_IS_MY_RC_NUMBER
        | WE_HAVE_RECIEVED_YOUR_ISSUE
        | THANK_YOU_FOR_WRITING_TO_US
        | RIDER
        | TRIP_ID
        | NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST
        | NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP
        | NEED_IT_TO_AUTOSTART_YOUR_APP
        | NEED_IT_TO_ENABLE_LOCATION
        | OVERLAY_TO_DRAW_OVER_APPLICATIONS
        | BATTERY_OPTIMIZATIONS
        | AUTO_START_APPLICATION_IN_BACKGROUND
        | LOCATION_ACCESS
        | STEP
        | PAID
        | ENTERED_WRONG_OTP
        | OTP_INVALID_FOR_THIS_VEHICLE_VARIANT
        | COPIED
        | BANK_NAME
        | AADHAR_DETAILS
        | AADHAR_NUMBER
        | FRONT_SIDE_IMAGE
        | BACK_SIDE_IMAGE
        | STILL_NOT_RESOLVED
        | CASE_TWO
        | NON_DISCLOUSER_AGREEMENT
        | DATA_COLLECTION_AUTHORITY
        | SOFTWARE_LICENSE
        | LOAD_MORE
        | ARE_YOU_SURE_YOU_WANT_TO_LOGOUT
        | GO_BACK
        | THANK_YOU_FOR_REGISTERING_US
        | UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU
        | ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE
        | EMPTY_RIDES
        | YOU_HAVE_NOT_TAKEN_A_TRIP_YET
        | BOOK_NOW
        | RESEND_OTP_IN
        | WE_NEED_ACCESS_TO_YOUR_LOCATION
        | YOUR_LOCATION_HELPS_OUR_SYSTEM String
        | NO_INTERNET_CONNECTION
        | PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN
        | TRY_AGAIN
        | GRANT_ACCESS
        | ENTER_REFERRAL_MOBILE_NUMBER
        | APPLY
        | HAVE_A_REFERRAL
        | ADD_HERE
        | REFERRAL_APPLIED
        | SMALLEDIT
        | ADD_DRIVING_LICENSE
        | HELP
        | INVALID_DL_NUMBER
        | DRIVING_LICENSE_NUMBER
        | ENTER_DL_NUMBER
        | SELECT_DATE_OF_BIRTH
        | DATE_OF_BIRTH
        | WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION
        | ENTER_MINIMUM_FIFTEEN_CHARACTERS
        | ADD_YOUR_FRIEND
        | PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE
        | VALIDATING
        | VERIFICATION_PENDING
        | VERIFICATION_FAILED
        | NO_DOC_AVAILABLE
        | ISSUE_WITH_DL_IMAGE
        | STILL_HAVE_SOME_DOUBT
        | ISSUE_WITH_RC_IMAGE
        | PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT
        | OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED
        | INVALID_DRIVING_LICENSE
        | LIMIT_EXCEEDED_FOR_DL_UPLOAD
        | INVALID_VEHICLE_REGISTRATION_CERTIFICATE
        | LIMIT_EXCEEDED_FOR_RC_UPLOAD
        | YOUR_DOCUMENTS_ARE_APPROVED
        | APPLICATION_STATUS
        | FOR_SUPPORT
        | CONTACT_US
        | IMAGE_VALIDATION_FAILED
        | IMAGE_NOT_READABLE
        | IMAGE_LOW_QUALITY
        | IMAGE_INVALID_TYPE
        | IMAGE_DOCUMENT_NUMBER_MISMATCH
        | IMAGE_EXTRACTION_FAILED
        | IMAGE_NOT_FOUND
        | IMAGE_NOT_VALID
        | DRIVER_ALREADY_LINKED
        | DL_ALREADY_UPDATED
        | RC_ALREADY_LINKED
        | RC_ALREADY_UPDATED
        | DL_ALREADY_LINKED
        | SOMETHING_WENT_WRONG
        | PICKUP
        | TRIP
        | CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER String
        | RE_ENTER_VEHICLE_REGISTRATION_NUMBER
        | RE_ENTER_DRIVING_LICENSE_NUMBER
        | UPDATED_AT
        | TRIP_COUNT
        | TODAYS_EARNINGS
        | BONUS_EARNED
        | WHAT_IS_NAMMA_YATRI_BONUS String
        | BONUS_PRIMARY_TEXT
        | BONUS_SECONDARY_TEXT
        | DATE_OF_REGISTRATION
        | SELECT_DATE_OF_REGISTRATION
        | DATE_OF_ISSUE
        | PROVIDE_DATE_OF_ISSUE_TEXT
        | PROVIDE_DATE_OF_REGISTRATION_TEXT
        | SELECT_DATE_OF_ISSUE
        | SAME_REENTERED_RC_MESSAGE
        | SAME_REENTERED_DL_MESSAGE
        | WHERE_IS_MY_ISSUE_DATE
        | WHERE_IS_MY_REGISTRATION_DATE
        | EARNINGS_CREDITED_IN_ACCOUNT
        | INVALID_PARAMETERS
        | UNAUTHORIZED
        | INVALID_TOKEN
        | SOME_ERROR_OCCURED_IN_OFFERRIDE
        | SELECT_VEHICLE_TYPE
        | RIDE
        | NO_LOCATION_UPDATE
        | GOT_IT_TELL_US_MORE
        | WRITE_A_COMMENT
        | HOW_WAS_YOUR_RIDE_WITH
        | RUDE_BEHAVIOUR
        | LONG_WAITING_TIME
        | DIDNT_COME_TO_PICUP_LOCATION
        | HELP_US_WITH_YOUR_REASON
        | MAX_CHAR_LIMIT_REACHED
        | SHOW_ALL_OPTIONS
        | UPDATE_REQUIRED
        | PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE
        | NOT_NOW
        | OF
        | DROP
        | PLEASE_WAIT
        | SETTING_YOU_OFFLINE
        | SETTING_YOU_ONLINE
        | SETTING_YOU_SILENT
        | VIEW_BREAKDOWN
        | APP_INFO
        | OTHER
        | VEHICLE_ISSUE
        | FARE_UPDATED
        | FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES
        | CONTINUE
        | CONFIRM_PASSWORD
        | DEMO_MODE
        | PASSWORD
        | ENTER_DEMO_MODE_PASSWORD
        | DEMO_MODE_DISABLED
        | ONLINE_VIA_DEMO_MODE
        | MORE
        | LESS
        | YOU_ARE_AT_PICKUP
        | WAITING_FOR_CUSTOMER
        | CUSTOMER_NOTIFIED
        | PICKUP_TOO_FAR
        | CUSTOMER_NOT_PICKING_CALL
        | TRAFFIC_JAM
        | CUSTOMER_WAS_RUDE
        | ALL_MESSAGES
        | MESSAGES
        | ADD_A_COMMENT
        | POST_COMMENT
        | ENTER_YOUR_COMMENT
        | NO_NOTIFICATIONS_RIGHT_NOW
        | NO_NOTIFICATIONS_RIGHT_NOW_DESC
        | ALERTS
        | YOUR_COMMENT
        | SHOW_MORE
        | LOAD_OLDER_ALERTS
        | CONTEST
        | YOUR_REFERRAL_CODE_IS_LINKED
        | YOU_CAN_NOW_EARN_REWARDS
        | COMING_SOON
        | COMING_SOON_DESCRIPTION
        | REFERRAL_CODE_NUMBER
        | REFERRAL_CODE_HINT
        | CONFIRM_REFERRAL_CODE
        | CONFIRM_REFERRAL_CODE_HINT
        | YOUR_REFERRAL_CODE
        | FIRST_REFERRAL_SUCCESSFUL
        | AWAITING_REFERRAL_RIDE
        | CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT
        | REFERRED_CUSTOMERS
        | ACTIVATED_CUSTOMERS
        | REFERRAL_CODE_LINKING
        | CONTACT_SUPPORT
        | CALL_SUPPORT
        | YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT String
        | REFERRAL_ENROLMENT
        | REFERRALS
        | LINK_REFERRAL_CODE
        | DRIVER_DETAILS
        | FOR_UPDATES_SEE_ALERTS
        | SHARE_OPTIONS
        | ENTER_PASSWORD
        | WELCOME_TEXT String
        | ABOUT_TEXT String
        | YOUR_VEHICLE
        | BOOKING_OPTIONS
        | CONFIRM_AND_CHANGE
        | MAKE_YOURSELF_AVAILABLE_FOR
        | OTP_
        | CHOOSE_LANGUAGE
        | RIDE_FARE
        | RIDE_DISTANCE
        | MESSAGE
        | START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS
        | START_YOUR_CHAT_WITH_THE_DRIVER
        | I_AM_ON_MY_WAY
        | GETTING_DELAYED_PLEASE_WAIT
        | UNREACHABLE_PLEASE_CALL_BACK
        | ARE_YOU_STARING
        | PLEASE_COME_SOON
        | OK_I_WILL_WAIT
        | I_HAVE_ARRIVED
        | PLEASE_COME_FAST_I_AM_WAITING
        | PLEASE_WAIT_I_WILL_BE_THERE
        | LOOKING_FOR_YOU_AT_PICKUP
        | SILENT
        | TRY_SILENT_MODE
        | SILENT_MODE_PROMPT
        | GO_SILENT
        | GO_ONLINE
        | GO_ONLINE_PROMPT
        | LIVE_DASHBOARD
        | CLICK_TO_ACCESS_YOUR_ACCOUNT
        | ADD_ALTERNATE_NUMBER
        | ENTER_ALTERNATE_MOBILE_NUMBER
        | PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER
        | ALTERNATE_MOBILE_NUMBER
        | REMOVE
        | REMOVE_ALTERNATE_NUMBER
        | ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER
        | YES_REMOVE_IT
        | NUMBER_REMOVED_SUCCESSFULLY
        | EDIT_ALTERNATE_MOBILE_NUMBER
        | NUMBER_ADDED_SUCCESSFULLY
        | NUMBER_EDITED_SUCCESSFULLY
        | ALTERNATE_MOBILE_OTP_LIMIT_EXCEED
        | ATTEMPTS_LEFT
        | WRONG_OTP
        | OTP_LIMIT_EXCEEDED
        | OTP_LIMIT_EXCEEDED_MESSAGE
        | TRY_AGAIN_LATER
        | ATTEMPT_LEFT
        | NUMBER_ALREADY_EXIST_ERROR
        | PLEASE_ASK_RIDER_FOR_THE_OTP
        | YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN
        | I_ARRIVED
        | ESTIMATED_RIDE_FARE
        | COMPLETE_ONBOARDING
        | PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS
        | RESOLVED_ISSUE
        | ONGOING_ISSUE
        | LOST_ITEM
        | RIDE_RELATED_ISSUE
        | APP_RELATED_ISSUE
        | FARE_RELATED_ISSUE
        | ISSUE_NUMBER
        | REMOVE_ISSUE
        | CALL_SUPPORT_NUMBER
        | YEARS_AGO
        | MONTHS_AGO
        | DAYS_AGO
        | HOURS_AGO
        | MIN_AGO
        | SEC_AGO
        | VERIFICATION_IS_TAKING_A_BIT_LONGER
        | DEMO
        | RIDE_RELATED
        | FARE
        | APP_RELATED
        | LOST_AND_FOUND
        | REPORT_LOST_ITEM
        | CORPORATE_ADDRESS String
        | CORPORATE_ADDRESS_DESCRIPTION String
        | CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL String
        | REGISTERED_ADDRESS String
        | REGISTERED_ADDRESS_DESCRIPTION String
        | REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL String
        | SELECT_THE_LANGUAGES_YOU_CAN_SPEAK
        | GENDER
        | SELECT_YOUR_GENDER
        | MALE
        | FEMALE
        | PREFER_NOT_TO_SAY
        | SET_NOW
        | COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES
        | UPDATE_NOW
        | CONFIRM
        | GENDER_UPDATED
        | ZONE_CANCEL_TEXT_DROP
        | ZONE_CANCEL_TEXT_PICKUP
        | RANKINGS
        | GETTING_THE_LEADERBOARD_READY
        | PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS
        | LAST_UPDATED
        | CONGRATULATIONS_YOU_ARE_RANK
        | YOU
        | DAILY
        | INACCURATE_DATE_AND_TIME
        | ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN
        | THE_CURRENT_DATE_AND_TIME_IS
        | GO_TO_SETTING
        | ACCEPT_RIDES_TO_ENTER_RANKINGS
        | OTP_HAS_BEEN_RESENT
        | OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP
        | OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
        | OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN
        | SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
        | INVALID_REFERRAL_CODE
        | ISSUE_REMOVED_SUCCESSFULLY
        | OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
        | TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
        | INVALID_REFERRAL_NUMBER
        | SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
        | WAIT_TIME
        | WAIT_TIMER
        | HOW_LONG_WAITED_FOR_PICKUP
        | CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE
        | OTHERS
        | ENTER_SECOND_SIM_NUMBER
        | ALTERNATE_NUMBER
        | LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER
        | ADD_ALTERNATE_NUMBER_IN_MEANTIME
        | OTP_RESEND_LIMIT_EXCEEDED
        | ALTERNATE_NUMBER_CANNOT_BE_ADDED
        | OTP_RESENT
        | SEDAN
        | SUV
        | HATCHBACK
        | AUTO_RICKSHAW
        | TAXI
        | TAXI_PLUS
        | MY_PROFILE
        | SETTINGS
        | REG_NUMBER
        | TYPE
        | MODEL_NAME
        | COLOUR
        | BADGES
        | EDIT_RC
        | DEACTIVATE_RC
        | ACTIVATE_RC
        | DELETE_RC
        | CALL_DRIVER
        | CALL_CUSTOMER_SUPPORT
        | ACTIVE_RC_ON_ANOTHER_DRIVER
        | CALL_DRIVER_OR_CONTACT_SUPPORT
        | SKIP
        | ACTIVE_STR 
        | INACTIVE_RC
        | CONFIRMATION_FOR_DELETING_RC
        | YES_DELETE
        | ADD_NEW_RC
        | CONNECT_CALL_ANONYMOUSLY
        | YES_ACTIVATE
        | YES_DEACTIVATE
        | CONFIRMATION_FOR_DEACTIVATING_RC
        | CONFIRMATION_FOR_ACTIVATING_RC
        | THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC
        | REMOVED
        | DEACTIVATED
        | IS_ACTIVE_NOW
        | SINGLE_RC_CANNOT_BE_DELETED
        | CANCELLATION_RATE
        | RIDES_CANCELLED
        | EARNINGS_MISSED
        | SUMMARY
        | NAMMA_BONUS
        | TRIPS_COMPLETED
        | LATE_NIGHT_TRIPS
        | ABOUT_ME
        | ABOUT_VEHICLE
        | ADD
        | YEARS_OLD
        | HOMETOWN
        | MISSED_OPPORTUNITY
        | EARNED_ON_APP String
        | TRAVELLED_ON_APP String
        | HOW_OLD_IS_YOUR_VEHICLE
        | ENTER_NAME_OF_VEHICLE
        | NEW_
        | WITH
        | TOTAL_MONEY_COLLECTED
        | FARE_EARNED_OF_THE_DAY
        | GST_PLUS_PAYABLE
        | TO_CONTINUE_USING_YATRI_SATHI
        | PAY
        | LATER
        | GREAT_JOB
        | FEE_BREAKUP
        | YATRI_SATHI_FEE_PAYABLE_FOR_DATE String
        | FEE_CORRESPONDING_TO_THE_DISTANCE
        | PLATFORM_FEE
        | GST
        | TOTAL_PAYABLE
        | GOT_IT
        | VIEW_DETAILS
        | PAYMENT_SUCCESSFUL
        | PAYMENT_PENDING
        | PAYMENT_FAILED
        | PAYMENT_PENDING_DESC
        | PAYMENT_FAILED_DESC String
        | WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS
        | CONTINUE_TAKING_RIDES
        | YOUR_PREVIOUS_PAYMENT_IS_PENDING
        | GOVERMENT_CHARGES
        | TODAY
        | OKAY
        | NO_PAYMENT_HISTORY_AVAILABLE
        | YOU_DONT_HAVE_ANY_PAYMENTS
        | ENTER_AADHAAR_NUMBER
        | ENTER_AADHAAR_DETAILS
        | ENTER_AADHAAR_OTP_
        | AADHAAR_LINKING_REQUIRED
        | AADHAAR_LINKING_REQUIRED_DESCRIPTION String
        | BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC
        | TERMS_AND_CONDITIONS_SHORT
        | OTP_SENT_TO_AADHAAR_NUMBER
        | ENTER_SIX_DIGIT_OTP
        | TC_TAIL
        | LINK_AADHAAR_ID
        | NO_MOBILE_NUMBER_REGISTERED
        | EXCEED_OTP_GENERATION_LIMIT
        | AADHAAR_NUMBER_NOT_EXIST
        | INVALID_OTP
        | NO_SHARE_CODE
        | WRONG_SHARE_CODE
        | INVALID_SHARE_CODE
        | SESSION_EXPIRED
        | OTP_ATTEMPT_EXCEEDED
        | UPSTREAM_INTERNAL_SERVER_ERROR
        | TRANSACTION_ALREADY_COMPLETED
        | GOTO_YOUR_NEAREST_BOOTH
        | AADHAAR_ALREADY_LINKED
        | OPTIONAL
        | DOWNLOAD_STATEMENT
        | SELECT_A_DATE_RANGE
        | FEE_PAYMENT_HISTORY
        | LANGUAGES_SPOKEN
        | VIEW_PAYMENT_HISTORY
        | RIDE_TYPE
        | RC_STATUS
        | RATED_BY_USERS1
        | RATED_BY_USERS2
        | MONTHS
        | RC_ADDED_SUCCESSFULLY
        | CALL_REQUEST_HAS_BEEN_PLACED
        | TRIP_DATE
        | OFFER_APPLIED
        | YOUR_EARNINGS
        | NUMBER_OF_RIDES
        | FARE_BREAKUP
        | MY_PLAN
        | YOUR_DUES
        | YOUR_DUES_DESCRIPTION
        | YOUR_DUES_DESCRIPTION_MANUAL
        | CURRENT_DUES
        | YOUR_LIMIT
        | DUE_DETAILS
        | AMOUNT
        | VIEW_DUE_DETAILS
        | SETUP_AUTOPAY
        | CURRENT_PLAN
        | ALTERNATE_PLAN
        | AUTOPAY_DETAILS
        | CANCEL_AUTOPAY_STR
        | WE_MIGHT_BE_LOST
        | EXEPERIENCING_ERROR
        | ENJOY_THESE_BENEFITS
        | CHOOSE_YOUR_PLAN String
        | SKIP_FOR_NOW
        | SEVEN_DAY_FREE_TRIAL_ACTIVATED
        | TAKE_UNLIMITED_RIDES_FOR_THE_NEXT_SEVEN_DAYS
        | EVERY_RIDE_AT_ZERO_COMMISSION
        | EARN_UPTO_PER_DAY
        | HOW_THIS_WORKS
        | SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST
        | GET_REMINDED_ABOUT_YOUR_PLAN_SETUP
        | FREE_TRIAL_REMINDER
        | PLAN_STARTS
        | EASY_AUTOMATIC_PAYMENTS_START
        | FREE_UNTIL
        | PER_RIDE
        | PER_DAY
        | OFFER
        | OFFERS
        | YOU_ARE_ON_THE_FREE_TRIAL
        | SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES
        | GET_FREE_TRAIL_UNTIL
        | CLEAR_DUES
        | PAYMENT_PENDING_ALERT
        | PAYMENT_PENDING_ALERT_DESC
        | LOW_ACCOUNT_BALANCE
        | LOW_ACCOUNT_BALANCE_DESC
        | OKAY_GOT_IT
        | LIMITED_TIME_OFFER
        | JOIN_NOW
        | AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE
        | MANUAL_PAYMENTS_WILL_APPEAR_HERE
        | MANUAL_PAYMENTS
        | NO_AUTOMATIC_PAYMENTS_DESC
        | NO_MANUAL_PAYMENTS_DESC
        | PAYMENT_HISTORY
        | PLAN
        | DAY
        | TAP_A_PLAN_TO_VIEW_DETAILS
        | PLANS
        | HOW_IT_WORKS
        | ZERO_COMMISION
        | EARN_TODAY_PAY_TOMORROW
        | PAY_ONLY_IF_YOU_TAKE_RIDES
        | MANAGE_PLAN
        | VIEW_AUTOPAY_DETAILS
        | SWITCH_AND_SAVE
        | SWITCH_AND_SAVE_DESC
        | SWITCH_NOW
        | PAYMENT_MODE_CHANGED_TO_MANUAL
        | PAYMENT_MODE_CHANGED_TO_MANUAL_DESC
        | AUTOPAY_PAYMENTS
        | SUCCESS
        | TRANSACTION_ON
        | DEBITED_ON
        | RIDES_TAKEN_ON
        | JOIN_PLAN
        | JOIN_NAMMAA_YATRI
        | CANCEL_AUTOPAY_AND_PAY_MANUALLY
        | PLAN_ACTIVATED_SUCCESSFULLY
        | DUES_CLEARED_SUCCESSFULLY
        | NOT_PLANNING_TO_TAKE_RIDES
        | RETRY_PAYMENT_STR
        | PAUSE_AUTOPAY_STR
        | SETUP_AUTOPAY_STR
        | VIEW_RIDE_DETAILS
        | ACCOUNT
        | AUTOPAY_IS_NOT_ENABLED_YET
        | ENABLE_AUTOPAY_DESC
        | ENABLE_AUTOPAY_NOW
        | AUTOPAY_SETUP_PENDING_STR
        | AUTOPAY_PENDING_DESC_STR
        | REFRESH_STR
        | TRANSACTION_DETAILS
        | RIDE_DETAILS
        | MY_PLAN_TITLE String
        | SWITCH_TO
        | PLEASE_TRY_AGAIN
        | PLAN_NOT_FOUND
        | MANDATE_NOT_FOUND
        | ACTIVE_MANDATE_EXISTS
        | NO_ACTIVE_MANDATE_EXIST
        | NO_PLAN_FOR_DRIVER
        | INVALID_PAYMENT_MODE
        | INVALID_AUTO_PAY_STATUS
        | MAX_AMOUNT
        | FREQUENCY
        | STATRED_ON
        | EXPIRES_ON
        | SWITCHED_PLAN
        | RESUMED_AUTOPAY
        | ONETIME
        | WEEKLY
        | FORTNIGHTLY
        | MONTHLY
        | BIMONTHLY
        | QUARTERLY
        | HALFYEARLY
        | YEARLY
        | ASPRESENTED
        | FIRST_FREE_RIDE
        | DAILY_PER_RIDE_DESC
        | JOIN_THE_UNLIMITED_PLAN
        | MAYBE_LATER
        | DO_YOU_WANT_TO_CANCEL
        | DO_YOU_WANT_TO_CANCEL_DESC
        | YOUR_PAYMENT_WAS_UNSUCCESSFUL
        | PAYMENT_CANCELLED
        | MANUAL_PAYMENT_STR
        | UPI_AUTOPAY_S
        | DAILY_UNLIMITED
        | DAILY_PER_RIDE
        | DAILY_UNLIMITED_PLAN_DESC
        | DAILY_PER_RIDE_PLAN_DESC
        | AUTOPAY_CANCELLED
        | NO
        | YES_CANCEL
        | PAY_TO_JOIN_THIS_PLAN
        | OFFERS_NOT_APPLICABLE
        | PAUSED_STR
        | PENDING_STR
        | SWITCH_PLAN_STR
        | OFFERS_APPLICABLE_ON_DAILY_UNLIMITED
        | DAILY_UNLIMITED_OFFER_NOT_AVAILABLE
        | PLAN_SWITCHED_TO
        | NO_RIDES_NO_CHARGE
        | GET_SPECIAL_OFFERS
        | VALID_ONLY_IF_PAYMENT
        | HELP_STR
        | REFRESH_STRING
        | CHAT_FOR_HELP
        | VIEW_FAQs
        | FIND_HELP_CENTRE
        | CONTACT
        | GO_TO_LOCATION
        | NO_HELP_CENTER_IS_ACTIVE_NOW
        | HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE
        | SUPPORT
        | NEED_HELP_JOINING_THE_PLAN
        | NEED_HELP
        | SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS
        | SETUP_NOW
        | GO_TO_VEHICLE_DETAILS 
        | CLOSE
        | RC_DEACTIVATED 
        | RC_DEACTIVATED_DETAILS 
        | CUSTOMER_HAS_LOW_MOBILITY
        | CUSTOMER_HAS_DISABILITY
        | CUSTOMER_HAS_LOW_VISION
        | CUSTOMER_HAS_HEARING_IMPAIRMENT
        | HELP_WITH_THEIR_MOBILITY_AID
        | PLEASE_ASSIST_THEM_IF_NEEDED
        | MESSAGE_THEM_AT_PICKUP
        | SOUND_HORN_ONCE_AT_PICKUP
        | PLEASE_CALL_AND_AVOID_CHATS
        | PLEASE_CHAT_AND_AVOID_CALLS
        | PLEASE_GO_TO_EXACT_PICKUP
        | CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP
        | CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP
        | CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP
        | CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM
        | CUSTOMER_MAY_NEED_ASSISTANCE
        | LEARN_MORE
        | CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC
        | CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING
        | CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING
        | PLEASE_HELP_THEM_AS_YOU_CAN
        | LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE
        | ASSISTANCE_REQUIRED
        | SAVED_DUE_TO_ZERO_COMMISSION
        | TIP_EARNED_FROM_CUSTOMER 
        | COLLECT_VIA_CASE_UPI
        | FARE_COLLECTED
        | RATE_YOUR_RIDE_WITH1
        | RATE_YOUR_RIDE_WITH2
        | HELP_US_WITH_YOUR_FEEDBACK
        | COLLECT_CASH 
        | ONLINE_PAYMENT
        | RIDE_COMPLETED
        | SUBMIT_FEEDBACK
        | BADGE_EARNED 
        | PURPLE_RIDE_CHAMPION 
        | PURPLE_RIDE 
        | PROCEED_TO_CHAT
        | PLEASE_CONSIDER_CALLING_THEM
        | JOIN_A_PLAN_TO_START_EARNING
        | GO_ONLINE_PROMPT_SUBSCRIBE
        | GO_ONLINE_PROMPT_PAYMENT_PENDING
        | COMPLETE_PAYMENT_TO_CONTINUE String
        | DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES
        | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1
        | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_2
        | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3
        | AC_CAB
        | AC_SUV
        | DOWNGRADE_VEHICLE
        | PENDING_CAPS
        | FAILURE
        | PAYMENT_MODE
        | TXN_ID
        | AMOUNT_PAID
        | NOTIFICATION_SCHEDULED
        | MANUAL_DUES
        | AUTOPAY_IN_PROGRESS
        | MANUAL_DUE_OVERVIEW
        | AUTOPAY_DUE_OVERVIEW
        | MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED
        | CLEAR_MANUAL_DUES
        | DUE_OVERVIEW 
        | MANUAL_DUE_DETAILS
        | AUTOPAY_DUE_DETAILS
        | SWITCHED_TO_MANUAL
        | SPLIT_PAYMENT
        | GST_INCLUDE
        | SCHEDULED_AT
        | PAYMENT_STATUS
        | NOTIFICATION_ATTEMPTING
        | EXECUTION_SCHEDULED
        | EXECUTION_ATTEMPTING
        | EXECUTION_SUCCESS
        | SCHEDULED
        | ONE_TIME_SETTLEMENT
        | PAYMENT_SCHEDULED
        | RETRY_AUTOPAY
        | RETRY_STR
        | ONGOING_PAYMENT_EXECUTION
        | OFFER_CARD_BANNER_TITLE String
        | OFFER_CARD_BANNER_DESC
        | OFFER_CARD_BANNER_ALERT
        | OR
        | COLLECT_CASH_DIRECTLY
        | OR_COLLECT_CASH_DIRECTLY
        | SETUP_AUTOPAY_TO_ACCEPT_PAYMENT
        | DOWNLOAD_QR
        | USE_THIS_QR_TO_COLLECT_PAYMENT
        | AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT
        | GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT
        | PAYMENT
        | QR_CODE
        | GET_QR_CODE
        | EXECUTION_FAILED
        | NOTIFICATION_FAILED
        | CLEAR_DUES_BANNER_TITLE
        | PAY_NOW
        | COLLECT_VIA_UPI_QR_OR_CASH
        | TRANSACTION_DEBITED_ON
        | TRANSACTION_ATTEMPTED_ON
        | AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL
        | AUTOPAY_SETUP_SUCCESSFUL
        | AUTOPAY_SETUP_AND_PAYMENT_PENDING
        | AUTOPAY_SETUP_PENDING
        | AUTOPAY_SETUP_AND_PAYMENT_FAILED
        | AUTOPAY_SETUP_FAILED
        | ONE_TIME_REGISTERATION
        | CLEARANCE_AND_REGISTERATION
        | UPI_AUTOPAY_SETUP
        | WATCH_VIDEO_FOR_HELP
        | PAYMENT_PENDING_SOFT_NUDGE
        | CLEAR_YOUR_DUES_EARLY
        | DUE_LIMIT_WARNING_BANNER_TITLE
        | SCHEDULED_ON
        | ATTEMPTED_ON
        | FREE_TRIAL_ENDING_IN_2_DAYS
        | FREE_TRIAL_ENDING_TOMORROW
        | FREE_TRIAL_ENDS_TONIGHT
        | JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES
        | SETUP_AUTOPAY_FOR_EASY_PAYMENTS
        | LOW_DUES_CLEAR_POPUP_DESC
        | DUES_PENDING
        | DAYS
        | ACTIVE_PLAN
        | WHAT_ARE_PURPLE_RIDES
        | ECONOMICAL
        | SPACIOUS
        | COMFY
        | PEOPLE
        | GO_TO
        | SELECT_ON_MAP
        | CONFIRM_LOCATION_STR
        | SAVE_LOCATION_STR
        | REMOVE_PREF_LOC
        | CONF_REMOVE_PREF_LOC
        | YES_REMOVE
        | ADD_LOCATION
        | ADD_ANOTHER_LOCATION
        | ADD_A_GOTO_LOC
        | GOTO_LOC_LEFT
        | CURRENT_LOCATION
        | CONF_GOTO_LOC
        | GOTO_LOCS
        | LOCATION_STR
        | ADD_TAG
        | ONLY_ONE_LOC_CAN_ADDED
        | SAVE_AS
        | NO_GOTO_LOC_ADDED
        | GOTO_LOC_HELPS_YOU
        | YOU_ARE_VERY_CLOSE
        | GOTO_IS_APPLICABLE_FOR
        | CANCEL_ANYWAY
        | GOTO_MAYBE_REDUCED
        | CANCEL_OF_GOTO
        | MORE_GOTO_RIDE_COMING
        | MORE_GOTO_RIDE_COMING_DESC
        | GOTO_REDUCED_TO_ZERO
        | DUE_TO_MULTIPLE_CANCELLATIONS
        | OK_GOT_IT
        | GOTO_REDUCED_TO
        | VALIDITY_EXPIRED_STR
        | VALIDITY_EXPIRED_DESC
        | KNOW_MORE
        | THIS_FEATURE_WILL_BE_APPLICABLE
        | GOTO_LOC_ADDED
        | GOTO_LOC_REMOVED
        | GOTO_LOC_UPDATED
        | GOTO_LOC_IS_ENABLED
        | GOTO_LOC_IS_DISABLED
        | GOTO_LOCATIONS
        | CHOOSE_A_GOTO_LOC
        | YOU_HAVE_ONLY_LEFT_FOR_TODAY
        | YES_ENABLE
        | NO_GOTO_LOCS_ADDED_YET
        | NO_GOTO_LOCS_ADDED_YET_DESC
        | ENABLE_GOTO
        | GO_TO_CANCELLATION_TITLE
        | GO_TO_CANCELLATION_DESC
        | DISABLE_GOTO_STR
        | YOU_STILL_HAVE_TIME_LEFT
        | YES_DISABLE
        | GOTO_LOC_REACHED
        | YOU_ARE_ALMOST_AT_LOCATION
        | DRIVER_HOME_LOCATION_NOT_FOUND
        | DRIVER_HOME_LOCATION_DOES_NOT_EXIST
        | DRIVER_HOME_LOCATION_LIMIT_REACHED
        | DRIVER_GO_HOME_REQUEST_NOT_FOUND
        | DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST
        | DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED
        | DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE
        | REPORT_ISSUE
        | DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA
        | NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION
        | DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER
        | DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR
        | DRAG_TO_ADJUST
        | LOCATION_ALREADY_EXISTS
        | MIN_LEFT
        | GET_READY_FOR_YS_SUBSCRIPTION String
        | SIGNUP_EARLY_FOR_SPECIAL_OFFERS
        | GUARANTEED_FIXED_PRICE String
        | INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON
        | NO_CHARGES_TILL
        | DRIVER_GO_HOME_REQUEST_NOT_PRESENT
        | AND
        | DIRECT_PAYMENT_NO_COMMISSIONS
        | CUSTOMER_PAYS_DIRECTLY
        | HUNDRED_PERCENT_FARE_GOES_TO_YOU
        | FARE_SHOWN_IS_FARE_YOU_GET
        | BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION
        | OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT
        | YOUR_DETECTED_LOCATION_IS
        | LANGUAGE_DETECTED
        | CHANGE_LANGUAGE_STR
        | SELECT_LOCATION
        | SELECT_LOCATION_DESC 
        | SELECT_LANGUAGE_DESC 
        | CONFIRM_LANGUAGE
        | GET_STARTED
        | ENABLE_LOCATION_PERMISSION
        | PLEASE_ENABLE_LOCATION_PERMISSION_FOR
        | ENABLE_LOCATION
        | BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR
        | ENTER_YOUR_MOBILE_NUMBER
        | NOTIFICATION_ACCESS
        | NOTIFICATION_ACCESS_DESC
        | WATCH_VIDEO
        | DL_VERIFICATION_FAILED
        | RC_VERIFICATION_FAILED
        | DL_UPLOAD_FAILED
        | RC_UPLOAD_FAILED
        | PLEASE_RETRY_THE_UPLOAD_AGAIN
        | RC_AND_DL_UPLOAD_FAILED
        | RC_UPLOAD_LIMIT_REACHED
        | DL_UPLOAD_LIMIT_REACHED
        | RETRY_UPLOAD
        | VEHICLE_REGISTERATON_CERTIFICATE
        | GRANT_PERMISSIONS
        | SUBSCRIPTION_PLAN_STR
        | COMPLETE_AUTOPAY_LATER
        | START_EARNING_IN_FOUR_STEPS
        | COMPLETE
        | HOW_TO_UPLOAD
        | TAKE_CLEAR_PICTURE_DL
        | ENSURE_ADEQUATE_LIGHT
        | FIT_DL_CORRECTLY
        | TAKE_PHOTO
        | FIT_RC_CORRECTLY
        | TAKE_CLEAR_PICTURE_RC
        | DL_UPLOADED 
        | RC_UPLOADED 
        | DL_UPLOADING 
        | RC_UPLOADING 
        | RETAKE_RC 
        | RETAKE_DL 
        | CONFIRM_AND_UPLOAD 
        | RETAKE_PHOTO 
        | CHANGE_CITY
        | LETS_GET_YOU_TRIP_READY
        | GOT_AN_OTP
        | DRIVING_LICENSE_DETAILS
        | VEHICLE_REGISTRATION_DETAILS
        | UPLOAD_REGISTRATION_CERTIFICATE_STR
        | UPLOAD_PHOTO
        | CLEAR_IMAGE
        | BLURRY_IMAGE
        | CROPPED_CORRECTLY
        | WRONG_CROPPING
        | CHANGE_LOCATION
        | RC_VERIFICATION_IN_PROGRESS
        | RC_VERIFICATION_FAILED_STATUS
        | RC_VERIFICATION_SUCCESS
        | RC_IN_PROGRESS_DESC
        | RC_FAILED_DESC
        | TAKE_A_PHOTO
        | GALLERY
        | UNABLE_TO_DETECT_YOUR_LOCATION
        | DETECTING_LOCATION
        | GET_FULL_PAYMENT
        | SELECT_CITY_STR
        | WE_ARE_NOT_LIVE_IN_YOUR_AREA
        | LOCATION_UNSERVICEABLE
        | UNABLE_TO_GET_YOUR_LOCATION
        | TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART
        | THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY
        | CLICK_TO_ADD_PROFILE_PICTURE
        | ADD_PROFILE_PHOTO
        | TAKE_SELFIE
        | YOUR_PHOTO_WILL_HELP_US_TO_VERIFY_IDENTITY
        | CONFIRM_SELFIE
        | RETAKE_SELFIE
        | SELFIE_VERIFIED
        | PLEASE_RETAKE_SELFIE
        | PROFILE_PICTURE_ADDED_SUCCESSFULLY
        | PROFILE_PICTURE_UPDATED_SUCCESSFULLY
        | UPDATE_PROFILE_PHOTO
        | FACE_NOT_DETECTED
        | FACE_NOT_CLEAR 
        | NEED_PERMISSION_TO_ACCESS_CAMERA
        | MULTIPLE_FACES
        | FACE_DETECTION_FAILED