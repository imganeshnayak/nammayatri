{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Language.Types where

data STR =
        AADHAAR_ALREADY_LINKED
       | AADHAAR_LINKING_REQUIRED
       | AADHAAR_LINKING_REQUIRED_DESCRIPTION String
       | AADHAAR_NUMBER_NOT_EXIST
       | AADHAR_CARD
       | AADHAR_DETAILS
       | AADHAR_NUMBER
       | ABOUT
       | ABOUT_APP_DESCRIPTION
       | ABOUT_ME
       | ABOUT_TEXT String
       | ABOUT_VEHICLE
       | AC_CAB
       | AC_SUV
       | ACCEPT_FOR
       | ACCEPT_RIDES_TO_ENTER_RANKINGS
       | ACCOUNT
       | ACTIVATE_RC
       | ACTIVATED_CUSTOMERS
       | ACTIVE_MANDATE_EXISTS
       | ACTIVE_PLAN
       | ACTIVE_RC_ON_ANOTHER_DRIVER
       | ACTIVE_STR
       | ADD
       | ADD_A_COMMENT
       | ADD_A_GOTO_LOC
       | ADD_ALTERNATE_NUMBER
       | ADD_ALTERNATE_NUMBER_IN_MEANTIME
       | ADD_ANOTHER
       | ADD_ANOTHER_LOCATION
       | ADD_BANK_DETAILS
       | ADD_DRIVING_LICENSE
       | ADD_HERE
       | ADD_IMAGE
       | ADD_LOCATION
       | ADD_NEW_RC
       | ADD_TAG
       | ADD_VEHICLE_DETAILS
       | ADD_VOICE_NOTE
       | ADD_YOUR_FRIEND
       | ADDED_IMAGES
       | ADDED_VOICE_NOTE
       | ADHAAR_INTRUCTION_PICTURE
       | ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN
       | ALERTS
       | ALL_MESSAGES
       | ALL_TOPICS
       | ALLOW_ACCESS
       | ALTERNATE_MOBILE_NUMBER
       | ALTERNATE_MOBILE_OTP_LIMIT_EXCEED
       | ALTERNATE_NUMBER
       | ALTERNATE_NUMBER_CANNOT_BE_ADDED
       | ALTERNATE_PLAN
       | AMOUNT
       | AMOUNT_PAID
       | AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT
       | AND
       | APP_INFO
       | APP_RELATED
       | APP_RELATED_ISSUE
       | APPLICATION_STATUS
       | APPLY
       | ARE_YOU_STARING
       | ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE
       | ARE_YOU_SURE_YOU_WANT_TO_LOGOUT
       | ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER
       | ASK_DETAILS_MESSAGE
       | ASK_DETAILS_MESSAGE_REVERSED
       | ASPRESENTED
       | ASSISTANCE_REQUIRED
       | ATTEMPT_LEFT
       | ATTEMPTED_ON
       | ATTEMPTS_LEFT
       | AUTO
       | AUTO_READING_OTP
       | AUTO_RICKSHAW
       | AUTO_START_APPLICATION_IN_BACKGROUND
       | AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE
       | AUTOPAY_CANCELLED
       | AUTOPAY_DETAILS
       | AUTOPAY_DUE_DETAILS
       | AUTOPAY_DUE_OVERVIEW
       | AUTOPAY_IN_PROGRESS
       | AUTOPAY_IS_NOT_ENABLED_YET
       | AUTOPAY_PAYMENTS
       | AUTOPAY_PENDING_DESC_STR
       | AUTOPAY_SETUP_AND_PAYMENT_FAILED
       | AUTOPAY_SETUP_AND_PAYMENT_PENDING
       | AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL
       | AUTOPAY_SETUP_FAILED
       | AUTOPAY_SETUP_PENDING
       | AUTOPAY_SETUP_PENDING_STR
       | AUTOPAY_SETUP_SUCCESSFUL
       | AWAITING_REFERRAL_RIDE
       | BACK_SIDE
       | BACK_SIDE_IMAGE
       | BADGE_EARNED
       | BADGES
       | BANK_DETAILS
       | BANK_NAME
       | BATTERY_OPTIMIZATIONS
       | BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION
       | BENIFICIARY_NUMBER
       | BIMONTHLY
       | BLURRY_IMAGE
       | BONUS_EARNED
       | BONUS_PRIMARY_TEXT
       | BONUS_SECONDARY_TEXT
       | BOOK_NOW
       | BOOKING_OPTIONS
       | BY_CASH
       | BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR
       | BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR
       | BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC
       | CALL
       | CALL_CUSTOMER_DESCRIPTION
       | CALL_CUSTOMER_SUPPORT
       | CALL_CUSTOMER_TITLE
       | CALL_DRIVER
       | CALL_DRIVER_OR_CONTACT_SUPPORT
       | CALL_REQUEST_HAS_BEEN_PLACED
       | CALL_SUPPORT
       | CALL_SUPPORT_CENTER
       | CALL_SUPPORT_NUMBER
       | CANCEL
       | CANCEL_ANYWAY
       | CANCEL_AUTOPAY_AND_PAY_MANUALLY
       | CANCEL_AUTOPAY_STR
       | CANCEL_OF_GOTO
       | CANCEL_RIDE
       | CANCELLATION_RATE
       | CANCELLED_
       | CASE_TWO
       | CASH_COLLECTED
       | CHANGE_CITY
       | CHANGE_LANGUAGE_STR
       | CHANGE_LOCATION
       | CHAT_FOR_HELP
       | CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT
       | CHOOSE_A_GOTO_LOC
       | CHOOSE_AN_OPTION
       | CHOOSE_LANGUAGE
       | CHOOSE_VEHICLE_TYPE
       | CHOOSE_YOUR_PLAN String
       | CLEAR_DUES
       | CLEAR_DUES_BANNER_TITLE
       | CLEAR_IMAGE
       | CLEAR_MANUAL_DUES
       | CLEAR_YOUR_DUES_EARLY
       | CLEARANCE_AND_REGISTERATION
       | CLICK_TO_ACCESS_YOUR_ACCOUNT
       | CLOSE
       | COLLECT_AMOUNT_IN_CASH
       | COLLECT_CASH
       | COLLECT_CASH_DIRECTLY
       | COLLECT_VIA_CASE_UPI
       | COLLECT_VIA_UPI_QR_OR_CASH
       | COLOUR
       | COMFY
       | COMING_SOON
       | COMING_SOON_DESCRIPTION
       | COMPLETE
       | COMPLETE_AUTOPAY_LATER
       | COMPLETE_ONBOARDING
       | COMPLETE_PAYMENT_TO_CONTINUE String
       | COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES
       | COMPLETED_
       | CONF_GOTO_LOC
       | CONF_REMOVE_PREF_LOC
       | CONFIRM
       | CONFIRM_AND_CHANGE
       | CONFIRM_AND_UPLOAD
       | CONFIRM_LANGUAGE
       | CONFIRM_LOCATION_STR
       | CONFIRM_PASSWORD
       | CONFIRM_REFERRAL_CODE
       | CONFIRM_REFERRAL_CODE_HINT
       | CONFIRMATION_FOR_ACTIVATING_RC
       | CONFIRMATION_FOR_DEACTIVATING_RC
       | CONFIRMATION_FOR_DELETING_RC
       | CONGRATULATIONS_YOU_ARE_RANK
       | CONNECT_CALL_ANONYMOUSLY
       | CONTACT
       | CONTACT_SUPPORT
       | CONTACT_US
       | CONTEST
       | CONTINUE
       | CONTINUE_TAKING_RIDES
       | COPIED
       | CORPORATE_ADDRESS String
       | CORPORATE_ADDRESS_DESCRIPTION String
       | CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL String
       | COUNTRY_CODE_INDIA
       | CROPPED_CORRECTLY
       | CURRENT_DUES
       | CURRENT_LOCATION
       | CURRENT_PLAN
       | CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER String
       | CUSTOMER_HAS_DISABILITY
       | CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM
       | CUSTOMER_HAS_HEARING_IMPAIRMENT
       | CUSTOMER_HAS_LOW_MOBILITY
       | CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC
       | CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP
       | CUSTOMER_HAS_LOW_VISION
       | CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING
       | CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING
       | CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP
       | CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP
       | CUSTOMER_MAY_NEED_ASSISTANCE
       | CUSTOMER_NOT_PICKING_CALL
       | CUSTOMER_NOTIFIED
       | CUSTOMER_PAYS_DIRECTLY
       | CUSTOMER_WAS_RUDE
       | CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE
       | DAILY
       | DAILY_PER_RIDE
       | DAILY_PER_RIDE_DESC
       | DAILY_PER_RIDE_PLAN_DESC
       | DAILY_UNLIMITED
       | DAILY_UNLIMITED_OFFER_NOT_AVAILABLE
       | DAILY_UNLIMITED_PLAN_DESC
       | DATA_COLLECTION_AUTHORITY
       | DATE_OF_BIRTH
       | DATE_OF_ISSUE
       | DATE_OF_REGISTRATION
       | DAY
       | DAYS
       | DAYS_AGO
       | DEACTIVATE_RC
       | DEACTIVATED
       | DEBITED_ON
       | DECLINE
       | DELETE
       | DELETE_RC
       | DEMO
       | DEMO_MODE
       | DEMO_MODE_DISABLED
       | DESCRIBE_YOUR_ISSUE
       | DETECTING_LOCATION
       | DIDNT_COME_TO_PICUP_LOCATION
       | DIDNT_RECIEVE_OTP
       | DIRECT_PAYMENT_NO_COMMISSIONS
       | DISABLE_GOTO_STR
       | DISTANCE
       | DL_ALREADY_LINKED
       | DL_ALREADY_UPDATED
       | DL_UPLOAD_FAILED
       | DL_UPLOAD_LIMIT_REACHED
       | DL_UPLOADED
       | DL_UPLOADING
       | DL_VERIFICATION_FAILED
       | DO_YOU_WANT_TO_CANCEL
       | DO_YOU_WANT_TO_CANCEL_DESC
       | DONE
       | DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES
       | DOWNGRADE_VEHICLE
       | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1
       | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_2
       | DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3
       | DOWNLOAD_QR
       | DOWNLOAD_STATEMENT
       | DRAG_TO_ADJUST
       | DRIVER_ALREADY_LINKED
       | DRIVER_DETAILS
       | DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE
       | DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED
       | DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST
       | DRIVER_GO_HOME_REQUEST_NOT_FOUND
       | DRIVER_GO_HOME_REQUEST_NOT_PRESENT
       | DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR
       | DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER
       | DRIVER_HOME_LOCATION_DOES_NOT_EXIST
       | DRIVER_HOME_LOCATION_LIMIT_REACHED
       | DRIVER_HOME_LOCATION_NOT_FOUND
       | DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA
       | DRIVING_LICENSE
       | DRIVING_LICENSE_DETAILS
       | DRIVING_LICENSE_NUMBER
       | DROP
       | DUE_DETAILS
       | DUE_LIMIT_WARNING_BANNER_TITLE
       | DUE_OVERVIEW
       | DUE_TO_MULTIPLE_CANCELLATIONS
       | DUES_CLEARED_SUCCESSFULLY
       | DUES_PENDING
       | EARN_TODAY_PAY_TOMORROW
       | EARN_UPTO_PER_DAY
       | EARNED_ON_APP String
       | EARNINGS_CREDITED_IN_ACCOUNT
       | EARNINGS_MISSED
       | EARNINGS_WILL_BE_CREDITED
       | EASY_AUTOMATIC_PAYMENTS_START
       | ECONOMICAL
       | EDIT
       | EDIT_ALTERNATE_MOBILE_NUMBER
       | EDIT_RC
       | EMPTY_RIDES
       | ENABLE_AUTOPAY_DESC
       | ENABLE_AUTOPAY_NOW
       | ENABLE_GOTO
       | ENABLE_LOCATION
       | ENABLE_LOCATION_PERMISSION
       | END_RIDE
       | ENJOY_THESE_BENEFITS
       | ENSURE_ADEQUATE_LIGHT
       | ENTER_AADHAAR_DETAILS
       | ENTER_AADHAAR_NUMBER
       | ENTER_AADHAAR_OTP_
       | ENTER_ACCOUNT_NUMBER
       | ENTER_ALTERNATE_MOBILE_NUMBER
       | ENTER_DEMO_MODE_PASSWORD
       | ENTER_DL_NUMBER
       | ENTER_DRIVING_LICENSE_NUMBER
       | ENTER_IFSC_CODE
       | ENTER_MINIMUM_FIFTEEN_CHARACTERS
       | ENTER_MOBILE_NUMBER
       | ENTER_MODEL_NAME
       | ENTER_NAME_OF_VEHICLE
       | ENTER_OTP
       | ENTER_OTP_SENT_TO
       | ENTER_PASSWORD
       | ENTER_RC_NUMBER
       | ENTER_REFERRAL_MOBILE_NUMBER
       | ENTER_SECOND_SIM_NUMBER
       | ENTER_SIX_DIGIT_OTP
       | ENTER_VEHICLE_COLOUR
       | ENTER_VEHICLE_NO
       | ENTER_YOUR_COMMENT
       | ENTER_YOUR_MOBILE_NUMBER
       | ENTERED_WRONG_OTP
       | ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
       | ESTIMATED_RIDE_FARE
       | EVERY_RIDE_AT_ZERO_COMMISSION
       | EXCEED_OTP_GENERATION_LIMIT
       | EXECUTION_ATTEMPTING
       | EXECUTION_FAILED
       | EXECUTION_SCHEDULED
       | EXECUTION_SUCCESS
       | EXEPERIENCING_ERROR
       | EXPIRES_ON
       | FAILURE
       | FARE
       | FARE_BREAKUP
       | FARE_COLLECTED
       | FARE_EARNED_OF_THE_DAY
       | FARE_RELATED_ISSUE
       | FARE_SHOWN_IS_FARE_YOU_GET
       | FARE_UPDATED
       | FEE_BREAKUP
       | FEE_CORRESPONDING_TO_THE_DISTANCE
       | FEE_PAYMENT_HISTORY
       | FEMALE
       | FILL_VEHICLE_DETAILS
       | FIND_HELP_CENTRE
       | FIRST_FREE_RIDE
       | FIRST_REFERRAL_SUCCESSFUL
       | FIT_DL_CORRECTLY
       | FIT_RC_CORRECTLY
       | FOLLOW_STEPS
       | FOR_OTHER_ISSUES_WRITE_TO_US
       | FOR_SUPPORT
       | FOR_UPDATES_SEE_ALERTS
       | FORTNIGHTLY
       | FREE_TRIAL_ENDING_IN_2_DAYS
       | FREE_TRIAL_ENDING_TOMORROW
       | FREE_TRIAL_ENDS_TONIGHT
       | FREE_TRIAL_REMINDER
       | FREE_UNTIL
       | FREQUENCY
       | FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES
       | FRONT_SIDE
       | FRONT_SIDE_IMAGE
       | GALLERY
       | GENDER
       | GENDER_UPDATED
       | GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT
       | GET_FREE_TRAIL_UNTIL
       | GET_FULL_PAYMENT
       | GET_QR_CODE
       | GET_READY_FOR_YS_SUBSCRIPTION String
       | GET_REMINDED_ABOUT_YOUR_PLAN_SETUP
       | GET_SPECIAL_OFFERS
       | GET_STARTED
       | GETTING_DELAYED_PLEASE_WAIT
       | GETTING_STARTED_AND_FAQ
       | GETTING_THE_LEADERBOARD_READY
       | GO_BACK
       | GO_HOME
       | GO_OFFLINE
       | GO_ONLINE
       | GO_ONLINE_POPUP
       | GO_ONLINE_PROMPT
       | GO_ONLINE_PROMPT_PAYMENT_PENDING
       | GO_ONLINE_PROMPT_SUBSCRIBE
       | GO_SILENT
       | GO_TO
       | GO_TO_CANCELLATION_DESC
       | GO_TO_CANCELLATION_TITLE
       | GO_TO_HOME
       | GO_TO_LOCATION
       | GO_TO_SETTING
       | GO_TO_VEHICLE_DETAILS
       | GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE
       | GOT_AN_OTP
       | GOT_IT
       | GOT_IT_TELL_US_MORE
       | GOTO_IS_APPLICABLE_FOR
       | GOTO_LOC_ADDED
       | GOTO_LOC_HELPS_YOU
       | GOTO_LOC_IS_DISABLED
       | GOTO_LOC_IS_ENABLED
       | GOTO_LOC_LEFT
       | GOTO_LOC_REACHED
       | GOTO_LOC_REMOVED
       | GOTO_LOC_UPDATED
       | GOTO_LOCATIONS
       | GOTO_LOCS
       | GOTO_MAYBE_REDUCED
       | GOTO_REDUCED_TO
       | GOTO_REDUCED_TO_ZERO
       | GOTO_YOUR_NEAREST_BOOTH
       | GOVERMENT_CHARGES
       | GRANT_ACCESS
       | GRANT_PERMISSIONS
       | GREAT_JOB
       | GST
       | GST_INCLUDE
       | GST_PLUS_PAYABLE
       | GUARANTEED_FIXED_PRICE String
       | HALFYEARLY
       | HATCHBACK
       | HAVE_A_REFERRAL
       | HELP
       | HELP_AND_FAQ
       | HELP_AND_SUPPORT
       | HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE
       | HELP_STR
       | HELP_US_WITH_YOUR_FEEDBACK
       | HELP_US_WITH_YOUR_REASON
       | HELP_WITH_THEIR_MOBILITY_AID
       | HOME
       | HOMETOWN
       | HOURS_AGO
       | HOW_IT_WORKS
       | HOW_LONG_WAITED_FOR_PICKUP
       | HOW_OLD_IS_YOUR_VEHICLE
       | HOW_THIS_WORKS
       | HOW_TO_UPLOAD
       | HOW_WAS_YOUR_RIDE_WITH
       | HUNDRED_PERCENT_FARE_GOES_TO_YOU
       | I_AM_ON_MY_WAY
       | I_ARRIVED
       | I_DONT_KNOW_WHICH_RIDE
       | I_HAVE_ARRIVED
       | IFSC_CODE
       | IMAGE_ADDED
       | IMAGE_DOCUMENT_NUMBER_MISMATCH
       | IMAGE_EXTRACTION_FAILED
       | IMAGE_INVALID_TYPE
       | IMAGE_LOW_QUALITY
       | IMAGE_NOT_FOUND
       | IMAGE_NOT_READABLE
       | IMAGE_NOT_VALID
       | IMAGE_PREVIEW
       | IMAGE_VALIDATION_FAILED
       | IMAGES_ADDED
       | INACCURATE_DATE_AND_TIME
       | INACTIVE_RC
       | INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON
       | INVALID_AUTO_PAY_STATUS
       | INVALID_DL_NUMBER
       | INVALID_DRIVING_LICENSE
       | INVALID_MOBILE_NUMBER
       | INVALID_OTP
       | INVALID_PARAMETERS
       | INVALID_PAYMENT_MODE
       | INVALID_REFERRAL_CODE
       | INVALID_REFERRAL_NUMBER
       | INVALID_SHARE_CODE
       | INVALID_TOKEN
       | INVALID_VEHICLE_REGISTRATION_CERTIFICATE
       | IS_ACTIVE_NOW
       | IS_WAITING_FOR_YOU
       | ISSUE_NO
       | ISSUE_NUMBER
       | ISSUE_REMOVED_SUCCESSFULLY
       | ISSUE_SUBMITTED_MESSAGE
       | ISSUE_SUBMITTED_TEXT
       | ISSUE_WITH_DL_IMAGE
       | ISSUE_WITH_RC_IMAGE
       | JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES
       | JOIN_A_PLAN_TO_START_EARNING
       | JOIN_NAMMAA_YATRI
       | JOIN_NOW
       | JOIN_PLAN
       | JOIN_THE_UNLIMITED_PLAN
       | KNOW_MORE
       | LANGUAGE_DETECTED
       | LANGUAGE_UPDATED
       | LANGUAGES
       | LANGUAGES_SPOKEN
       | LAST_UPDATED
       | LATE_NIGHT_TRIPS
       | LATER
       | LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE
       | LEARN_MORE
       | LESS
       | LETS_GET_STARTED
       | LETS_GET_YOU_TRIP_READY
       | LICENSE_INSTRUCTION_CLARITY
       | LICENSE_INSTRUCTION_PICTURE
       | LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER
       | LIMIT_EXCEEDED_FOR_DL_UPLOAD
       | LIMIT_EXCEEDED_FOR_RC_UPLOAD
       | LIMITED_TIME_OFFER
       | LINK_AADHAAR_ID
       | LINK_REFERRAL_CODE
       | LIVE_DASHBOARD
       | LOAD_MORE
       | LOAD_OLDER_ALERTS
       | LOADING
       | LOCATION_ACCESS
       | LOCATION_ALREADY_EXISTS
       | LOCATION_STR
       | LOCATION_UNSERVICEABLE
       | LOGO
       | LOGOUT
       | LONG_WAITING_TIME
       | LOOKING_FOR_YOU_AT_PICKUP
       | LOST_AND_FOUND
       | LOST_ITEM
       | LOW_ACCOUNT_BALANCE
       | LOW_ACCOUNT_BALANCE_DESC
       | LOW_DUES_CLEAR_POPUP_DESC
       | MAKE_YOURSELF_AVAILABLE_FOR
       | MALE
       | MANAGE_PLAN
       | MANDATE_NOT_FOUND
       | MANDATORY
       | MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED
       | MANUAL_DUE_DETAILS
       | MANUAL_DUE_OVERVIEW
       | MANUAL_DUES
       | MANUAL_PAYMENT_STR
       | MANUAL_PAYMENTS
       | MANUAL_PAYMENTS_WILL_APPEAR_HERE
       | MAPS
       | MAX_AMOUNT
       | MAX_CHAR_LIMIT_REACHED
       | MAX_IMAGES
       | MAYBE_LATER
       | MESSAGE
       | MESSAGE_THEM_AT_PICKUP
       | MESSAGES
       | MIN_AGO
       | MIN_LEFT
       | MISSED_OPPORTUNITY
       | MOBILE_NUMBER
       | MODEL_NAME
       | MONTHLY
       | MONTHS
       | MONTHS_AGO
       | MORE
       | MORE_GOTO_RIDE_COMING
       | MORE_GOTO_RIDE_COMING_DESC
       | MORE_OPTIONS
       | MY_PLAN
       | MY_PLAN_TITLE String
       | MY_PROFILE
       | MY_RIDES
       | NAME
       | NAMMA_BONUS
       | NEED_HELP
       | NEED_HELP_JOINING_THE_PLAN
       | NEED_IT_TO_AUTOSTART_YOUR_APP
       | NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP
       | NEED_IT_TO_ENABLE_LOCATION
       | NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST
       | NEW_
       | NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION
       | NEXT
       | NO
       | NO_ACTIVE_MANDATE_EXIST
       | NO_AUTOMATIC_PAYMENTS_DESC
       | NO_CHARGES_TILL
       | NO_DOC_AVAILABLE
       | NO_GOTO_LOC_ADDED
       | NO_GOTO_LOCS_ADDED_YET
       | NO_GOTO_LOCS_ADDED_YET_DESC
       | NO_HELP_CENTER_IS_ACTIVE_NOW
       | NO_IMAGES_ADDED
       | NO_INTERNET_CONNECTION
       | NO_LOCATION_UPDATE
       | NO_MANUAL_PAYMENTS_DESC
       | NO_MOBILE_NUMBER_REGISTERED
       | NO_NOTIFICATIONS_RIGHT_NOW
       | NO_NOTIFICATIONS_RIGHT_NOW_DESC
       | NO_PAYMENT_HISTORY_AVAILABLE
       | NO_PLAN_FOR_DRIVER
       | NO_RIDES_NO_CHARGE
       | NO_SHARE_CODE
       | NO_VOICE_NOTE_ADDED
       | NON_DISCLOUSER_AGREEMENT
       | NOT_NOW
       | NOT_PLANNING_TO_TAKE_RIDES
       | NOTE
       | NOTIFICATION_ACCESS
       | NOTIFICATION_ACCESS_DESC
       | NOTIFICATION_ATTEMPTING
       | NOTIFICATION_FAILED
       | NOTIFICATION_SCHEDULED
       | NUMBER_ADDED_SUCCESSFULLY
       | NUMBER_ALREADY_EXIST_ERROR
       | NUMBER_EDITED_SUCCESSFULLY
       | NUMBER_OF_RIDES
       | NUMBER_REMOVED_SUCCESSFULLY
       | OF
       | OFFER
       | OFFER_APPLIED
       | OFFER_CARD_BANNER_ALERT
       | OFFER_CARD_BANNER_DESC
       | OFFER_CARD_BANNER_TITLE String
       | OFFERS
       | OFFERS_APPLICABLE_ON_DAILY_UNLIMITED
       | OFFERS_NOT_APPLICABLE
       | OFFLINE
       | OK_GOT_IT
       | OK_I_WILL_WAIT
       | OKAY
       | OKAY_GOT_IT
       | ONE_TIME_REGISTERATION
       | ONE_TIME_SETTLEMENT
       | ONETIME
       | ONGOING_ISSUE
       | ONGOING_ISSUES
       | ONGOING_PAYMENT_EXECUTION
       | ONLINE_
       | ONLINE_PAYMENT
       | ONLINE_VIA_DEMO_MODE
       | ONLY_ONE_LOC_CAN_ADDED
       | OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED
       | OPTIONAL
       | OR
       | OR_COLLECT_CASH_DIRECTLY
       | OTHER
       | OTHERS
       | OTP_
       | OTP_ATTEMPT_EXCEEDED
       | OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
       | OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP
       | OTP_HAS_BEEN_RESENT
       | OTP_INVALID_FOR_THIS_VEHICLE_VARIANT
       | OTP_LIMIT_EXCEEDED
       | OTP_LIMIT_EXCEEDED_MESSAGE
       | OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN
       | OTP_RESEND_LIMIT_EXCEEDED
       | OTP_RESENT
       | OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
       | OTP_SENT_TO
       | OTP_SENT_TO_AADHAAR_NUMBER
       | OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT
       | OVERLAY_TO_DRAW_OVER_APPLICATIONS
       | PAID
       | PASSWORD
       | PAUSE_AUTOPAY_STR
       | PAUSED_STR
       | PAY
       | PAY_NOW
       | PAY_ONLY_IF_YOU_TAKE_RIDES
       | PAY_TO_JOIN_THIS_PLAN
       | PAYMENT
       | PAYMENT_CANCELLED
       | PAYMENT_FAILED
       | PAYMENT_FAILED_DESC String
       | PAYMENT_HISTORY
       | PAYMENT_MODE
       | PAYMENT_MODE_CHANGED_TO_MANUAL
       | PAYMENT_MODE_CHANGED_TO_MANUAL_DESC
       | PAYMENT_PENDING
       | PAYMENT_PENDING_ALERT
       | PAYMENT_PENDING_ALERT_DESC
       | PAYMENT_PENDING_DESC
       | PAYMENT_PENDING_SOFT_NUDGE
       | PAYMENT_SCHEDULED
       | PAYMENT_STATUS
       | PAYMENT_SUCCESSFUL
       | PENDING_CAPS
       | PENDING_STR
       | PEOPLE
       | PER_DAY
       | PER_RIDE
       | PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS
       | PERSONAL_DETAILS
       | PICKUP
       | PICKUP_TOO_FAR
       | PLACE_CALL
       | PLACE_CALL_REQUEST
       | PLAN
       | PLAN_ACTIVATED_SUCCESSFULLY
       | PLAN_NOT_FOUND
       | PLAN_STARTS
       | PLAN_SWITCHED_TO
       | PLANS
       | PLATFORM_FEE
       | PLEASE_ASK_RIDER_FOR_THE_OTP
       | PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP
       | PLEASE_ASSIST_THEM_IF_NEEDED
       | PLEASE_CALL_AND_AVOID_CHATS
       | PLEASE_CHAT_AND_AVOID_CALLS
       | PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT
       | PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN
       | PLEASE_COME_FAST_I_AM_WAITING
       | PLEASE_COME_SOON
       | PLEASE_CONSIDER_CALLING_THEM
       | PLEASE_ENABLE_LOCATION_PERMISSION_FOR
       | PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER
       | PLEASE_ENTER_VALID_OTP
       | PLEASE_GO_TO_EXACT_PICKUP
       | PLEASE_HELP_THEM_AS_YOU_CAN
       | PLEASE_RETRY_THE_UPLOAD_AGAIN
       | PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
       | PLEASE_TRY_AGAIN
       | PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE
       | PLEASE_WAIT
       | PLEASE_WAIT_I_WILL_BE_THERE
       | PLEASE_WAIT_WHILE_IN_PROGRESS
       | PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE
       | PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS
       | POST_COMMENT
       | PREFER_NOT_TO_SAY
       | PREVIEW
       | PRIVACY_POLICY
       | PROCEED_TO_CHAT
       | PROFILE
       | PROGRESS_SAVED
       | PROVIDE_DATE_OF_ISSUE_TEXT
       | PROVIDE_DATE_OF_REGISTRATION_TEXT
       | PURPLE_RIDE
       | PURPLE_RIDE_CHAMPION
       | QR_CODE
       | QUARTERLY
       | RANKINGS
       | RATE_YOUR_RIDE_WITH1
       | RATE_YOUR_RIDE_WITH2
       | RATED_BY_USERS1
       | RATED_BY_USERS2
       | RC_ADDED_SUCCESSFULLY
       | RC_ALREADY_LINKED
       | RC_ALREADY_UPDATED
       | RC_AND_DL_UPLOAD_FAILED
       | RC_DEACTIVATED
       | RC_DEACTIVATED_DETAILS
       | RC_FAILED_DESC
       | RC_IN_PROGRESS_DESC
       | RC_STATUS
       | RC_UPLOAD_FAILED
       | RC_UPLOAD_LIMIT_REACHED
       | RC_UPLOADED
       | RC_UPLOADING
       | RC_VERIFICATION_FAILED
       | RC_VERIFICATION_FAILED_STATUS
       | RC_VERIFICATION_IN_PROGRESS
       | RC_VERIFICATION_SUCCESS
       | RE_ENTER_BENIFICIARY_NUMBER
       | RE_ENTER_DRIVING_LICENSE_NUMBER
       | RE_ENTER_VEHICLE_REGISTRATION_NUMBER
       | RECORD_VOICE_NOTE
       | REFERRAL_APPLIED
       | REFERRAL_CODE_HINT
       | REFERRAL_CODE_LINKING
       | REFERRAL_CODE_NUMBER
       | REFERRAL_ENROLMENT
       | REFERRALS
       | REFERRED_CUSTOMERS
       | REFRESH_STR
       | REFRESH_STRING
       | REG_NUMBER
       | REGISTER
       | REGISTERED_ADDRESS String
       | REGISTERED_ADDRESS_DESCRIPTION String
       | REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL String
       | REGISTRATION
       | REGISTRATION_CERTIFICATE_IMAGE
       | REGISTRATION_STEPS
       | REMOVE
       | REMOVE_ALTERNATE_NUMBER
       | REMOVE_ISSUE
       | REMOVE_PREF_LOC
       | REMOVED
       | REPORT_AN_ISSUE
       | REPORT_AN_ISSUE_WITH_THIS_TRIP
       | REPORT_ISSUE
       | REPORT_ISSUE_CHAT_PLACEHOLDER String
       | REPORT_LOST_ITEM
       | REQUEST
       | RESEND_OTP
       | RESEND_OTP_IN
       | RESOLVED_ISSUE
       | RESOLVED_ISSUES
       | RESUMED_AUTOPAY
       | RETAKE_DL
       | RETAKE_PHOTO
       | RETAKE_RC
       | RETRY_AUTOPAY
       | RETRY_PAYMENT_STR
       | RETRY_STR
       | RETRY_UPLOAD
       | RIDE
       | RIDE_COMPLETED
       | RIDE_COMPLETED_WITH
       | RIDE_DETAILS
       | RIDE_DISTANCE
       | RIDE_FARE
       | RIDE_RELATED
       | RIDE_RELATED_ISSUE
       | RIDE_REPORT_ISSUE
       | RIDE_TYPE
       | RIDER
       | RIDES
       | RIDES_CANCELLED
       | RIDES_TAKEN_ON
       | RUDE_BEHAVIOUR
       | SAME_REENTERED_DL_MESSAGE
       | SAME_REENTERED_RC_MESSAGE
       | SAVE_AS
       | SAVE_LOCATION_STR
       | SAVED_DUE_TO_ZERO_COMMISSION
       | SCHEDULED
       | SCHEDULED_AT
       | SCHEDULED_ON
       | SEC_AGO
       | SEDAN
       | SELECT_A_DATE_RANGE
       | SELECT_CITY_STR
       | SELECT_DATE_OF_BIRTH
       | SELECT_DATE_OF_ISSUE
       | SELECT_DATE_OF_REGISTRATION
       | SELECT_LANGUAGE
       | SELECT_LANGUAGE_DESC
       | SELECT_LOCATION
       | SELECT_LOCATION_DESC
       | SELECT_ON_MAP
       | SELECT_OPTION
       | SELECT_OPTION_REVERSED
       | SELECT_THE_LANGUAGES_YOU_CAN_SPEAK
       | SELECT_VEHICLE_TYPE
       | SELECT_YOUR_GENDER
       | SENDING_OTP
       | SESSION_EXPIRED
       | SET_NOW
       | SETTING_YOU_OFFLINE
       | SETTING_YOU_ONLINE
       | SETTING_YOU_SILENT
       | SETTINGS
       | SETUP_AUTOPAY
       | SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES
       | SETUP_AUTOPAY_FOR_EASY_PAYMENTS
       | SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS
       | SETUP_AUTOPAY_STR
       | SETUP_AUTOPAY_TO_ACCEPT_PAYMENT
       | SETUP_NOW
       | SEVEN_DAY_FREE_TRIAL_ACTIVATED
       | SHARE_OPTIONS
       | SHOW_ALL_OPTIONS
       | SHOW_MORE
       | SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST
       | SIGNUP_EARLY_FOR_SPECIAL_OFFERS
       | SILENT
       | SILENT_MODE_PROMPT
       | SINGLE_RC_CANNOT_BE_DELETED
       | SKIP
       | SKIP_FOR_NOW
       | SMALLEDIT
       | SOFTWARE_LICENSE
       | SOME_ERROR_OCCURED_IN_OFFERRIDE
       | SOMETHING_WENT_WRONG
       | SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
       | SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
       | SOUND_HORN_ONCE_AT_PICKUP
       | SPACIOUS
       | SPLIT_PAYMENT
       | START_EARNING_IN_FOUR_STEPS
       | START_RIDE
       | START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS
       | START_YOUR_CHAT_WITH_THE_DRIVER
       | STATRED_ON
       | STEP
       | STILL_HAVE_SOME_DOUBT
       | STILL_NOT_RESOLVED
       | SUBJECT
       | SUBMIT
       | SUBMIT_FEEDBACK
       | SUBMIT_ISSUE_DETAILS
       | SUBSCRIPTION_PLAN_STR
       | SUCCESS
       | SUMMARY
       | SUPPORT
       | SUV
       | SWITCH_AND_SAVE
       | SWITCH_AND_SAVE_DESC
       | SWITCH_NOW
       | SWITCH_PLAN_STR
       | SWITCH_TO
       | SWITCHED_PLAN
       | SWITCHED_TO_MANUAL
       | T_C
       | TAKE_A_PHOTO
       | TAKE_CLEAR_PICTURE_DL
       | TAKE_CLEAR_PICTURE_RC
       | TAKE_PHOTO
       | TAKE_UNLIMITED_RIDES_FOR_THE_NEXT_SEVEN_DAYS
       | TAP_A_PLAN_TO_VIEW_DETAILS
       | TAXI
       | TAXI_PLUS
       | TC_TAIL
       | TERMS_AND_CONDITIONS
       | TERMS_AND_CONDITIONS_SHORT
       | THANK_YOU_FOR_REGISTERING_US
       | THANK_YOU_FOR_WRITING_TO_US
       | THANK_YOU_FOR_WRTITTING_US
       | THE_CURRENT_DATE_AND_TIME_IS
       | THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY
       | THIS_FEATURE_WILL_BE_APPLICABLE
       | THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC
       | TIME_TAKEN
       | TIP_EARNED_FROM_CUSTOMER
       | TO_CONTINUE_USING_YATRI_SATHI
       | TODAY
       | TODAYS_EARNINGS
       | TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
       | TOTAL_MONEY_COLLECTED
       | TOTAL_PAYABLE
       | TRAFFIC_JAM
       | TRANSACTION_ALREADY_COMPLETED
       | TRANSACTION_ATTEMPTED_ON
       | TRANSACTION_DEBITED_ON
       | TRANSACTION_DETAILS
       | TRANSACTION_ON
       | TRAVELLED_ON_APP String
       | TRIP
       | TRIP_COUNT
       | TRIP_DATE
       | TRIP_DETAILS
       | TRIP_ID
       | TRIPS_COMPLETED
       | TRY_AGAIN
       | TRY_AGAIN_LATER
       | TRY_SILENT_MODE
       | TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART
       | TXN_ID
       | TYPE
       | UNABLE_TO_DETECT_YOUR_LOCATION
       | UNABLE_TO_GET_YOUR_LOCATION
       | UNAUTHORIZED
       | UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU
       | UNREACHABLE_PLEASE_CALL_BACK
       | UPDATE
       | UPDATE_NOW
       | UPDATE_REQUIRED
       | UPDATE_VEHICLE_DETAILS
       | UPDATED_AT
       | UPI_AUTOPAY_S
       | UPI_AUTOPAY_SETUP
       | UPLOAD_ADHAAR_CARD
       | UPLOAD_BACK_SIDE
       | UPLOAD_DRIVING_LICENSE
       | UPLOAD_FRONT_BACK
       | UPLOAD_FRONT_SIDE
       | UPLOAD_PHOTO
       | UPLOAD_RC
       | UPLOAD_REGISTRATION_CERTIFICATE
       | UPLOAD_REGISTRATION_CERTIFICATE_STR
       | UPSTREAM_INTERNAL_SERVER_ERROR
       | USE_THIS_QR_TO_COLLECT_PAYMENT
       | VALID_ONLY_IF_PAYMENT
       | VALIDATING
       | VALIDITY_EXPIRED_DESC
       | VALIDITY_EXPIRED_STR
       | VEHICLE_COLOUR
       | VEHICLE_DETAILS
       | VEHICLE_ISSUE
       | VEHICLE_MODEL_NAME
       | VEHICLE_REGISTERATON_CERTIFICATE
       | VEHICLE_REGISTRATION_DETAILS
       | VEHICLE_REGISTRATION_NUMBER
       | VEHICLE_TYPE
       | VERIFICATION_FAILED
       | VERIFICATION_IS_TAKING_A_BIT_LONGER
       | VERIFICATION_PENDING
       | VIEW
       | VIEW_ALL_RIDES
       | VIEW_AUTOPAY_DETAILS
       | VIEW_BREAKDOWN
       | VIEW_DETAILS
       | VIEW_DUE_DETAILS
       | VIEW_FAQs
       | VIEW_PAYMENT_HISTORY
       | VIEW_RIDE_DETAILS
       | VIEW_STATUS
       | VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS
       | VOICE_NOTE_ADDED
       | WAIT_TIME
       | WAIT_TIMER
       | WAITING_FOR_CUSTOMER
       | WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION
       | WATCH_VIDEO
       | WATCH_VIDEO_FOR_HELP
       | WE_ARE_NOT_LIVE_IN_YOUR_AREA
       | WE_HAVE_RECIEVED_YOUR_ISSUE
       | WE_MIGHT_BE_LOST
       | WE_NEED_ACCESS_TO_YOUR_LOCATION
       | WE_NEED_SOME_ACCESS
       | WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS
       | WEEKLY
       | WELCOME_TEXT String
       | WHAT_ARE_PURPLE_RIDES
       | WHAT_IS_NAMMA_YATRI_BONUS String
       | WHERE_IS_MY_ISSUE_DATE
       | WHERE_IS_MY_LICENSE_NUMBER
       | WHERE_IS_MY_RC_NUMBER
       | WHERE_IS_MY_REGISTRATION_DATE
       | WHICH_LANGUAGE_DO_YOU_PREFER
       | WITH
       | WRITE_A_COMMENT
       | WRITE_TO_US
       | WRONG_CROPPING
       | WRONG_OTP
       | WRONG_SHARE_CODE
       | YATRI_SATHI_FEE_PAYABLE_FOR_DATE String
       | YEARLY
       | YEARS_AGO
       | YEARS_OLD
       | YES_ACTIVATE
       | YES_CANCEL
       | YES_DEACTIVATE
       | YES_DELETE
       | YES_DISABLE
       | YES_ENABLE
       | YES_REMOVE
       | YES_REMOVE_IT
       | YOU
       | YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT String
       | YOU_ARE_ALMOST_AT_LOCATION
       | YOU_ARE_AT_PICKUP
       | YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS
       | YOU_ARE_OFFLINE
       | YOU_ARE_ON_A_RIDE
       | YOU_ARE_ON_THE_FREE_TRIAL
       | YOU_ARE_VERY_CLOSE
       | YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE
       | YOU_CAN_NOW_EARN_REWARDS
       | YOU_DONT_HAVE_ANY_PAYMENTS
       | YOU_HAVE_NOT_TAKEN_A_TRIP_YET
       | YOU_HAVE_ONLY_LEFT_FOR_TODAY
       | YOU_RATED
       | YOU_STILL_HAVE_TIME_LEFT
       | YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION
       | YOUR_COMMENT
       | YOUR_DETECTED_LOCATION_IS
       | YOUR_DOCUMENTS_ARE_APPROVED
       | YOUR_DUES
       | YOUR_DUES_DESCRIPTION
       | YOUR_DUES_DESCRIPTION_MANUAL
       | YOUR_EARNINGS
       | YOUR_EMAIL_ID
       | YOUR_LIMIT
       | YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN
       | YOUR_LOCATION_HELPS_OUR_SYSTEM String
       | YOUR_PAYMENT_WAS_UNSUCCESSFUL
       | YOUR_PREVIOUS_PAYMENT_IS_PENDING
       | YOUR_RECENT_RIDE
       | YOUR_RECENT_TRIP
       | YOUR_REFERRAL_CODE
       | YOUR_REFERRAL_CODE_IS_LINKED
       | YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN
       | YOUR_VEHICLE
       | ZERO_COMMISION
       | ZONE_CANCEL_TEXT_DROP
       | ZONE_CANCEL_TEXT_PICKUP
