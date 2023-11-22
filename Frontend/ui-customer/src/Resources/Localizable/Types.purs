{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Language.Types where

data STR
  = ABOUT
 | ABOUT_APP_DESCRIPTION String
 | ABOUT_REFERRAL_PROGRAM
 | ABOUT_REFERRAL_PROGRAM_DISCRIPTION String
 | ACCOUNT_DELETION_CONFIRMATION
 | ADD_ANOTHER_CONTACT
 | ADD_EMERGENCY_CONTACTS
 | ADD_FAVOURITE
 | ADD_NEW_ADDRESS
 | ADD_NEW_FAVOURITE
 | ADD_NOW
 | ADD_SAVED_LOCATION_FROM_SETTINGS
 | ADD_TAG
 | ADDRESS
 | ADDRESSES
 | ALL_FAVOURITES
 | ALL_TOPICS
 | ALREADY_EXISTS
 | ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION
 | AMOUNT_PAID
 | ANONYMOUS_CALL
 | ARE_YOU_STARING
 | ARE_YOU_SURE_YOU_WANT_TO_CANCEL
 | ARE_YOU_SURE_YOU_WANT_TO_LOGOUT
 | ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT
 | ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_
 | ASK_FOR_PRICE
 | ASK_FOR_PRICE_INFO
 | ASKED_FOR_MORE_MONEY
 | AT_DROP
 | AT_PICKUP
 | AUTO_ACCEPTING_SELECTED_RIDE
 | AUTO_ASSIGN_A_RIDE
 | AUTO_ASSIGN_DRIVER
 | AWAY
 | AWAY_C
 | BASE_FARES
 | BOARD_THE_FIRST
 | BOOK_NOW
 | BOOK_RIDE_
 | BOOKING_PREFERENCE
 | BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS
 | BY_CASH
 | BY_TAPPING_CONTINUE
 | CALL
 | CALL_DRIVER
 | CALL_DRIVER_USING
 | CALL_EMERGENCY_CONTACTS
 | CALL_EMERGENCY_CENTRE
 | CANCEL_
 | CANCEL_AUTO_ASSIGNING
 | CANCEL_ONGOING_SEARCH
 | CANCEL_RIDE
 | CANCEL_SEARCH
 | CANCEL_STR
 | CANCELLED
 | CHANGE
 | CHANGE_DROP_LOCATION
 | CHANGE_LOCATION
 | CHECK_OUT_LIVE_STATS
 | CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN
 | CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT
 | CHOOSE_BETWEEN_MULTIPLE_DRIVERS
 | CHOOSE_BETWEEN_MULTIPLE_RIDES
 | CHOOSE_ON_MAP
 | CHOOSE_YOUR_RIDE
 | COMFY
 | CONFIRM_AND_BOOK
 | CONFIRM_AND_SAVE
 | CONFIRM_CHANGES
 | CONFIRM_DROP_LOCATION
 | CONFIRM_EMERGENCY_CONTACTS
 | CONFIRM_FOR
 | CONFIRM_LOCATION
 | CONFIRM_PICKUP_LOCATION
 | CONFIRM_RIDE_
 | CONFIRMING_THE_RIDE_FOR_YOU
 | CONTACT_SUPPORT
 | CONTACTS_SELECTED
 | CONTINUE
 | COPIED
 | COULD_NOT_CONNECT_TO_DRIVER
 | CURRENT_LOCATION
 | CURRENTLY_WE_ARE_LIVE_IN_ String
 | CUSTOMER_SELECTED_FARE
 | CUSTOMER_TIP_DESCRIPTION
 | DIAL_112
 | DATA_COLLECTION_AUTHORITY
 | DAY_TIME_CHARGES
 | DAY_TIMES_OF
 | DAYTIME_CHARGES_APPLICABLE_AT_NIGHT
 | DAYTIME_CHARGES_APPLIED_AT_NIGHT
 | DEL_ACCOUNT
 | DELETE
 | DENY_ACCESS
 | DESCRIBE_YOUR_ISSUE
 | DESTINATION_OUTSIDE_LIMITS
 | DIRECT_CALL
 | DO_YOU_NEED_EMERGENCY_HELP
 | DOWNLOAD_PDF
 | DRAG_THE_MAP
 | DRIVER_PICKUP_CHARGES String
 | DRIVER_REQUESTED_TO_CANCEL
 | DRIVER_WAS_NOT_REACHABLE
 | DRIVER_WAS_RUDE
 | DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO
 | DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE
 | DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC
 | DRIVER_ADDITION_LIMITS_ARE_IN_INCREMENTS
 | DROP
 | DROP_LOCATION_FAR_AWAY
 | EARLY_END_RIDE_CHARGES
 | EARLY_END_RIDE_CHARGES_DESCRIPTION
 | ECONOMICAL
 | EDIT
 | EDIT_FAVOURITE
 | EMAIL
 | EMAIL_ALREADY_EXISTS
 | EMAIL_ID
 | EMERGENCY_CONTACS_ADDED_SUCCESSFULLY
 | EMERGENCY_CONTACTS
 | EMERGENCY_CONTACTS_SCREEN_DESCRIPTION
 | EMERGENCY_HELP
 | EMPTY_RIDES
 | ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE
 | ENJOY_RIDING_WITH_US
 | ENTER_4_DIGIT_OTP
 | ENTER_A_LOCATION
 | ENTER_MOBILE_NUMBER
 | ENTER_OTP
 | ENTER_YOUR_MOBILE_NUMBER
 | ENTER_YOUR_NAME
 | ERROR_404
 | ERROR_OCCURED_TRY_AGAIN
 | ESTIMATES_CHANGED
 | ESTIMATES_REVISED_TO
 | ETA_WAS_TOO_LONG
 | ETA_WAS_TOO_SHORT
 | EXISTS_AS
 | EXPIRES_IN
 | FAQ
 | FARE_UPDATED
 | FARE_WAS_HIGH
 | FAVOURITE
 | FAVOURITE_ADDED_SUCCESSFULLY
 | FAVOURITE_LOCATION
 | FAVOURITE_REMOVED_SUCCESSFULLY
 | FAVOURITE_UPDATED_SUCCESSFULLY
 | FAVOURITE_YOUR_CURRENT_LOCATION
 | FAVOURITES
 | FEMALE
 | FINDING_RIDES_NEAR_YOU
 | FOR_OTHER_ISSUES_WRITE_TO_US
 | FULL_NAME
 | GENDER_STR
 | GET_ESTIMATE_FARE
 | GETTING_DELAYED_PLEASE_WAIT
 | GETTING_ESTIMATES_FOR_YOU
 | GETTING_STARTED_AND_FAQS
 | GIVE_THIS_LOCATION_A_NAME
 | GO_BACK_
 | GO_HOME_
 | GO_TO_HOME__
 | GOOGLE_MAP_
 | GOT_ANOTHER_RIDE_ELSE_WHERE
 | GOT_IT
 | GOT_IT_TELL_US_MORE
 | GOVERNMENT_CHAGRES
 | GRANT_ACCESS
 | CGST
 | HAVE_REFERRAL_CODE
 | HATCHBACK
 | HELP_AND_SUPPORT
 | HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL
 | HELP_US_WITH_YOUR_REASON
 | HEY
 | HOME
 | HOPE_YOUR_RIDE_WAS_HASSLE_FREE
 | HOW_DO_YOU_IDENTIFY_YOURSELF
 | HOW_SHOULD_WE_ADDRESS_YOU
 | HOW_THE_PRICING_WORKS
 | HOW_THIS_WORKS
 | HOW_WAS_YOUR_RIDE_EXPERIENCE
 | HOW_WAS_YOUR_RIDE_WITH
 | ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN
 | I_AM_ON_MY_WAY
 | I_HAVE_ARRIVED
 | IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE
 | IN
 | IN_APP_TRACKING
 | INVALID_CODE_PLEASE_RE_ENTER
 | INVALID_MOBILE_NUMBER
 | INVOICE
 | IS_ON_THE_WAY
 | IS_WAITING_FOR_YOU
 | IT_SEEMS_TO_BE_A_VERY_BUSY_DAY
 | LANGUAGE
 | LET_TRY_THAT_AGAIN
 | LIVE_STATS_DASHBOARD
 | LOAD_MORE
 | LOADING
 | LOCATION
 | LOCATION_ALREADY
 | LOCATION_ALREADY_EXISTS
 | LOCATION_ALREADY_EXISTS_AS
 | LOCATION_UNSERVICEABLE
 | LOGIN_USING_THE_OTP_SENT_TO
 | LOGO
 | LOGOUT_
 | LOOKING_FOR_YOU_AT_PICKUP
 | LOST_SOMETHING
 | MALE
 | MANDATORY
 | MAX_CHAR_LIMIT_REACHED
 | MAYBE_LATER
 | MESSAGE
 | METERS_AWAY_FROM_YOUR_DESTINATION
 | MIN_FARE_UPTO
 | MORE_THAN
 | MINS_AWAY
 | MOBILE
 | MOBILE_NUMBER_STR
 | MY_RIDES
 | NAME
 | NAME_ALREADY_IN_USE
 | NAVIGATE
 | NEARBY
 | NIGHT_TIME_CHARGES
 | NIGHT_TIMES_OF
 | NO
 | NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD
 | NO_DONT
 | NO_EMERGENCY_CONTACTS_SET
 | NO_FAVOURITES_SAVED_YET
 | NO_MORE_RIDES
 | NO_TIP
 | NOMINAL_FARE
 | NOT_NOW
 | NOTE
 | NOTIFY_ME
 | OF
 | OK_I_WILL_WAIT
 | ONLINE_
 | OTHER
 | OTHERS
 | OTP
 | OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS
 | PAID
 | PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI
 | PAY_DRIVER_USING_CASH_OR_UPI
 | PAY_DRIVER_USING_CASH_OR_UPI_
 | PAY_THE_DRIVER
 | PAY_THE_DRIVER_INFO
 | PAY_THE_DRIVER_NOTE
 | PAY_VIA_CASH_OR_UPI
 | PAYMENT_METHOD
 | PAYMENT_METHOD_STRING
 | PAYMENT_METHOD_STRING_
 | PEOPLE
 | PERCENTAGE_OF_NOMINAL_FARE
 | PERSONAL_DETAILS
 | PICK_UP_LOCATION
 | PICK_UP_LOCATION_INCORRECT
 | PICKUP_AND_DROP
 | PICKUP_CHARGE
 | PLACE_CALL
 | PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE
 | PLEASE_COME_FAST_I_AM_WAITING
 | PLEASE_COME_SOON
 | PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH
 | PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
 | PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE
 | PLEASE_WAIT_I_WILL_BE_THERE
 | PLEASE_WAIT_WHILE_IN_PROGRESS
 | PREFER_NOT_TO_SAY
 | PRIVACY_POLICY
 | PROBLEM_AT_OUR_END
 | PROFILE_COMPLETION
 | PROMOTION
 | QUOTE_EXPIRED
 | RATE_ABOVE_MIN_FARE
 | RATE_CARD
 | RATE_YOUR_DRIVER
 | RATE_YOUR_RIDE
 | RATE_YOUR_RIDE_WITH
 | REFEREAL_CODE_DISCRIPTION
 | REFERRAL_CODE_APPLIED
 | REFERRAL_CODE_SUCCESSFULL
 | REGISTER_USING_DIFFERENT_NUMBER
 | REMOVE
 | REMOVE_FAVOURITE
 | REPEAT_RIDE
 | REPORT_AN_ISSUE
 | REPORT_AN_ISSUE_WITH_THIS_TRIP
 | REQUEST_AUTO_RIDE String
 | REQUEST_CALLBACK
 | REQUEST_RIDE
 | REQUEST_SUBMITTED
 | REQUEST_TO_DELETE_ACCOUNT
 | RESEND
 | RIDE_COMPLETED
 | RIDE_DETAILS
 | RIDE_FARE
 | RIDE_ID
 | RIDE_NOT_SERVICEABLE
 | APP_NOT_SERVICEABLE
 | SAVE
 | SAVE_AS
 | SAVE_PLACE
 | SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY
 | SAVED_ADDRESSES
 | SEARCH_AGAIN_WITH
 | SEARCH_AGAIN_WITH_A_TIP
 | SEARCH_AGAIN_WITHOUT_A_TIP
 | SEARCH_CONTACTS
 | SELECT_A_RIDE
 | SELECT_AN_OFFER
 | SELECT_AN_OFFER_FROM_OUR_DRIVERS
 | SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO
 | SELECT_CONTACTS
 | SELECT_FAVOURITE
 | SELECT_ON_MAP
 | SELECT_YOUR_DROP
 | SELECT_YOUR_GENDER
 | SEND_EMAIL
 | SERVICE_CHARGES
 | SET_LOCATION_ON_MAP
 | SET_NOW
 | SET_UP_YOUR_ACCOUNT
 | SHARE_APP
 | SHARE_RIDE_WITH_EMERGENCY_CONTACTS
 | SHOW_ALL_OPTIONS
 | SIX_DIGIT_REFERRAL_CODE
 | SKIP
 | SOFTWARE_LICENSE
 | SORRY_WE_COULDNT_FIND_ANY_RIDES
 | SORT_BY
 | SPACIOUS
 | START_
 | START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS
 | START_YOUR_CHAT_WITH_THE_DRIVER
 | STEPS_TO_COMPLETE
 | SUBJECT
 | SUBMIT
 | SUBMIT_FEEDBACK
 | SUCCESSFUL_ONBOARD String
 | SUPPORT
 | SUV
 | SEDAN
 | T_AND_C_A
 | TERMS_AND_CONDITIONS
 | THANK_YOU_FOR_WRITING
 | THANK_YOU_FOR_WRITING_TO_US
 | THANK_YOUR_DRIVER
 | THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE
 | TIP
 | TO_THE
 | TOTAL_AMOUNT
 | TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE
 | TOTAL_PAID
 | TRACK_LIVE_LOCATION_USING
 | TRIP_CHARGES
 | TRIP_DETAILS_
 | TRIP_ID
 | TRY_AGAIN
 | TRY_AGAIN_WITH
 | TRY_AGAIN_WITH_A_TIP
 | TRY_AGAIN_WITHOUT_TIP
 | TRY_CONNECTING_WITH_THE_DRIVER
 | TRY_LOOKING_FOR_RIDES_AGAIN
 | UNREACHABLE_PLEASE_CALL_BACK
 | UPDATE
 | UPDATE_PERSONAL_DETAILS
 | SETUP_NOW
 | NAMMA_SAFETY
 | UPDATE_REQUIRED
 | USE_CURRENT_LOCATION
 | USER
 | VERIFYING_OTP
 | VIEW_ALL_RIDES
 | VIEW_BREAKDOWN
 | VIEW_DETAILS
 | VIEW_INVOICE
 | VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS
 | WAIT_TIME
 | WAIT_TIME_TOO_LONG
 | WAITING_CHARGE
 | WAITING_CHARGE_DESCRIPTION
 | WE_HAVE_RECEIVED_YOUR_ISSUE
 | WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME
 | WE_NEED_ACCESS_TO_YOUR_LOCATION
 | WE_WILL_DELETE_YOUR_ACCOUNT
 | WELCOME_TEXT String
 | WHERE_TO
 | WORK
 | WRITE_A_COMMENT
 | WRITE_TO_US
 | WRONG_OTP
 | YES
 | YES_CANCEL_SEARCH
 | YES_DELETE_IT
 | YES_REMOVE
 | YES_TRY_AGAIN
 | YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT String
 | YOU_ARE_ABOUT_TO_CALL_NEAREST_EMERGENCY_CENTRE
 | YOU_ARE_OFFLINE
 | YOU_CAN_CANCEL_RIDE
 | YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE
 | YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER String
 | YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING
 | YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL
 | YOU_HAVENT_TAKEN_A_TRIP_YET
 | YOU_RATED
 | YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS
 | YOUR_EMAIL_ID
 | LOCATION_PERMISSION_SUBTITLE
 | YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER
 | YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE
 | YOUR_RECENT_RIDE
 | YOUR_RIDE_HAS_STARTED
 | YOUR_RIDE_IS_NOW_COMPLETE
 | YOUR_RIDES
 | YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST
 | DOWNLOAD_INVOICE
 | WAS_YOUR_CALL_SUCCESSFUL
 | DRIVER_ADDITIONS
 | FARE_UPDATE_POLICY
 | DRIVER_ADDITIONS_OPTIONAL
 | THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC
 | DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE String
 | DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE
 | YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS
 | REASON_CHANGE_IN_ROUTE_A
 | REASON_CHANGE_IN_ROUTE_B
 | GO_TO_ZONE String
 | REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON
 | CONTACT_REMOVED_SUCCESSFULLY
 | CORPORATE_ADDRESS
 | CORPORATE_ADDRESS_DESCRIPTION String
 | CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL String
 | REGISTERED_ADDRESS
 | REGISTERED_ADDRESS_DESCRIPTION String
 | REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL String
 | RECOMMENDED
 | COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE
 | COMPLETE_YOUR_NAMMA_SAFETY_SETUP_FOR_SAFE_RIDE_EXPERIENCE
 | UPDATE_NOW
 | WE_WOULD_APPRECIATE_YOUR_FEEDBACK
 | REASON_FOR_DELETING_ACCOUNT
 | SUBMIT_REQUEST
 | PLEASE_ENTER_A_VALID_EMAIL
 | WE_WOULD_APPRECIATE_YOUR_REASONING
 | OK_GOT_IT
 | WAIT_FOR_DRIVER
 | NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS
 | CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP
 | DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP
 | DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION
 | THE_PICKUP_LOCATION_ENTERED_WAS_WRONG
 | YOUR_DRIVER_IS_JUST
 | M_AWAY
 | DRIVER_HAS_ALREADY_TRAVELLED
 | PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING
 | CHANGE_OF_PLANS
 | DRIVER_IS_NOT_MOVING
 | WRONG_PICKUP_LOCATION
 | DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE
 | DRIVER_IS_NOT_MOVING_Q
 | WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING
 | DRIVER_IS_NEAR_YOUR_LOCATION
 | SOME_OTHER_REASON
 | LOCATION_PERMISSION_SUBTITLE_NEW_USER String
 | METRO_RIDE
 | GO_BACK_TEXT
 | DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST
 | DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST
 | AND_HAS_TRAVELLED
 | PLEASE_FIND_REVISED_FARE_ESTIMATE
 | FARE_ESTIMATE
 | TIP_SELECTED
 | ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
 | IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
 | CONTINUE_SEARCH_WITH
 | CONTINUING_SEARCH_WITH
 | SEARCHING_WITH
 | THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION
 | DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION
 | ALLOW_LOCATION_ACCESS
 | MESSAGE_FROM_DRIVER
 | REPLY
 | NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS
 | THIS_FIELD_IS_REQUIRED
 | EMAIL_EXISTS_ALREADY
 | OKAY_GOT_IT
 | CALL_NAMMA_YATRI_SUPPORT String
 | CALL_112
 | SEATS
 | OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN
 | OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
 | TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
 | SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
 | SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES
 | IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
 | CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
 | NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
 | OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN String
 | NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED
 | PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED
 | LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED
 | INVALID_CONTACT_FORMAT
 | OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
 | RATE_YOUR_EXPERIENCE
 | REPORT_ISSUE_
 | DONE
 | PLEASE_TELL_US_WHAT_WENT_WRONG
 | YOUR_FEEDBACK_HELPS_US String
 | DID_YOU_FACE_ANY_ISSUE
 | WE_NOTICED_YOUR_RIDE_ENDED_AWAY
 | GET_CALLBACK_FROM_US
 | DRIVER_WAS_NOT_READY_TO_GO
 | ASKING_FOR_MORE_MONEY
 | AUTO_BROKEN
 | WE_WILL_GIVE_YOU_CALLBACK
 | YOUR_ISSUE_HAS_BEEN_REPORTED
 | OTP_RESENT_SUCCESSFULLY
 | DESCRIPTION_SHOULD_BE_MORE_THAN_10_ALPHABETIC_CHARACTERS
 | INCORRECT_OTP_PLEASE_TRY_AGAIN
 | N_MORE_ATTEMPTS_LEFT
 | GO_TO_SELECTED_PICKUP_SPOT
 | GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED
 | UNPROFESSIONAL_DRIVER
 | RASH_DRIVING
 | DRIVER_CHARGED_MORE
 | UNCOMFORTABLE_AUTO
 | TRIP_GOT_DELAYED
 | FELT_UNSAFE
 | POLITE_DRIVER
 | EXPERT_DRIVING
 | SAFE_RIDE
 | CLEAN_AUTO
 | ON_TIME
 | SKILLED_NAVIGATOR
 | RUDE_DRIVER
 | TOO_MANY_CALLS
 | RECKLESS_DRIVING
 | LATE_DROP_OFF
 | LATE_PICK_UP
 | POOR_EXPERIENCE
 | TERRIBLE_EXPERIENCE
 | NEEDS_IMPROVEMENT
 | AMAZING
 | ALMOST_PERFECT
 | ASKED_FOR_EXTRA_FARE
 | ANYTHING_THAT_YOU_WOULD_LIKE_TO_TELL_US
 | PLATFORM_FEE
 | FINDING_QUOTES_TEXT
 | PLEASE_WAIT
 | PAY_DRIVER_USING_WALLET
 | FASTER
 | NEW_
 | SGST
 | OTP_EXPIRED
 | OTP_EXPIRED_DESCRIPTION
 | PLATFORM_GST
 | MISC_WAITING_CHARGE
 | TAXI_FROM_ZONE String
 | TAXI
 | AC
 | NON_AC
 | AC_TAXI
 | NON_AC_TAXI
 | GET_OTP_VIA_WHATSAPP
 | OR
 | HELPS_DRIVER_CONFIRM_ITS_YOU
 | LETS_GET_YOU_TRIP_READY
 | GOT_AN_OTP
 | JUST_ONE_LAST_THING
 | TOLL_CHARGES_WILL_BE_EXTRA
 | AUTO_RICKSHAW
 | CABS_AVAILABLE
 | GENERAL_DISABILITY_DESCRIPTION
 | PI_POINTER_1
 | PI_POINTER_2
 | VI_POINTER_1
 | VI_POINTER_2
 | HI_POINTER_1
 | HI_POINTER_2
 | ACCESSIBILITY_TEXT String
 | TO_CATER_YOUR_SPECIFIC_NEEDS String
 | SPECIAL_ASSISTANCE
 | SELECT_THE_CONDITION_THAT_IS_APPLICABLE
 | DISABILITY_CLAIMER_TEXT
 | ARE_YOU_A_PERSON_WITH_DISABILITY
 | DO_YOU_NEEED_SPECIAL_ASSISTANCE
 | ASSISTANCE_REQUIRED
 | NO_DISABILITY
 | LEARN_HOW_TEXT String
 | UPDATE_PROFILE
 | NOW_GET_ASSISTED_RIDES
 | SENT_OTP_VIA_SMS
 | SENT_OTP_VIA_WHATSAPP 
 | PLEASE_ENABLE_LOCATION_PERMISSION String
 | ENABLE_LOCATION_PERMISSION_TO
 | AC_SUV
 | AC_CAB
 | RIDE_TYPE
 | ERNAKULAM_LIMIT_CHARGE
 | SELECT_LOCATION_ON_MAP
 | DOWNLOAD_DRIVER_RECEIPT
 | VIEW_DRIVER_RECEIPT
 | DRIVER_RECEIPT
 | HELP
 | FARE_INFO_TEXT String
 | EDUCATIONAL_POP_UP_SLIDE_1_TITLE
 | EDUCATIONAL_POP_UP_SLIDE_2_TITLE
 | EDUCATIONAL_POP_UP_SLIDE_3_TITLE
 | EDUCATIONAL_POP_UP_SLIDE_4_TITLE
 | EDUCATIONAL_POP_UP_SLIDE_5_TITLE
 | EDUCATIONAL_POP_UP_SLIDE_1_SUBTITLE
 | EDUCATIONAL_POP_UP_SLIDE_2_SUBTITLE
 | EDUCATIONAL_POP_UP_SLIDE_3_SUBTITLE
 | EDUCATIONAL_POP_UP_SLIDE_4_SUBTITLE
 | EDUCATIONAL_POP_UP_SLIDE_5_SUBTITLE
 | INCLUSIVE_AND_ACCESSIBLE
 | YOU_SEEM_TO_BE_FAR_FROM_PICK_UP
 | ARE_YOU_SURE_YOU_WANT_TO_PROCEED_WITH_THE_BOOKING
 | MY_TICKETS
 | SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
 | YOU_CAN_BOOK_TICKETS_TO_THE_ZOO_BY_CLICKING_THE_BUTTON
 | PLACES_YOU_MIGHT_LIKE_TO_GO_TO
 | SUGGESTED_DESTINATION
 | RECENT_RIDES
 | ONE_CLICK_BOOKING_FOR_YOUR_FAVOURITE_JOURNEYS
 | VIEW_MORE
 | VIEW_LESS
 | HAVE_A_REFFERAL 
 | YOUR_SUGGESTED_DESTINATIONS_AND_RECENT_RIDES_WILL_APPEAR_HERE
 | WELCOME_TO_NAMMA_YATRI_
 | BOOK_AND_MOVE
 | ANYWHERE_IN_THE_CITY
 | CHECKOUT_OUR_LIVE_STATS
 | BENGALURU_MOST_LOVED_APP
 | PICKUP_
 | PAST_SEARCHES
 | SEARCH_RESULTS
 | REQUESTING_RIDE_IN
 | REQUESTING_RIDE
 | TAP_HERE_TO_STOP_AUTO_REQUESTING
 | POWERED_BY
 | BOOK_YOUR_RIDE
 | LEARN_MORE
 | LEARN_ABOUT_NAMMA_SAFETY
 | NAMMA_SAFETY_WILL_ENABLE_ACCESS
 | EDIT_ACTIONS
 | EMERGENCY_ACTIONS
 | WHEN_YOU_START_EMERGENCY_SOS
 | EMERGENCY_SHARING_WITH_CONTACTS
 | SHARING_WITH
 | ADD_A_CONTACT
 | TO_ENSURE_SAFETY_USERS_SHOULD
 | ABOUT_SOS_DESC
 | FEW_EXAMPLES_OF_SOS_SITUATIONS
 | THINGS_TO_DO_DURING_SOS_SITUATION
 | EMERGENCY_REQUEST_SENT
 | SOS_TRIGGERED_DESC
 | SOS_ACTIONS
 | CALL_POLICE
 | CALL_SUPPORT
 | RECORD_VIDEO
 | STOP_AND_SHARE_RECORDING
 | CANCEL_SHARING
 | START_RECORDING
 | SHARING_THE_VIDEO_IN
 | EMERGENCY_INFO_SHARED
 | EMERGENCY_INFO_SHARED_ACTION
 | SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS
 | SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM
 | ACTIVATE_LIVE_VIDEO_RECORDING_FEATURES
 | GET_OPTIONS_TO_DIRECTLY_CALL_SUPPORT
 | SHARE_LOCATION_AND_RIDE_DETAILS_EMERGENCY_CONTACT
 | NAMMA_SAFETY_MEASURES
 | SAFETY_GUIDELINES_FOR_YOU
 | ABOUT_SOS
 | TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT
 | NIGHT_TIME_SAFETY_CHECKS
 | SHARE_INFO_WITH_EMERGENCY_CONTACTS_TITLE
 | SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC
 | TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE
 | TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_DESC
 | ENABLE_NIGHT_TIME_SAFETY_ALERTS_TITLE
 | ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC
 | ALMOST_DONE_TITLE
 | ALMOST_DONE_DESC
 | SAFETY_MEASURE_1
 | SAFETY_MEASURE_2
 | SAFETY_MEASURE_3
 | SAFETY_MEASURE_4
 | SAFETY_MEASURE_5
 | SAFETY_MEASURE_6
 | SAFETY_GUIDELINES_1
 | SAFETY_GUIDELINES_2
 | SAFETY_GUIDELINES_3
 | SAFETY_GUIDELINES_4
 | SAFETY_GUIDELINES_5
 | SAFETY_GUIDELINES_6
 | SAFETY_GUIDELINES_7
 | ABOUT_SOS_1
 | ABOUT_SOS_2
 | ABOUT_SOS_3
 | ABOUT_SOS_4
 | ABOUT_SOS_5
 | ABOUT_SOS_6
 | ABOUT_SOS_7
 | ABOUT_SOS_8
 | ABOUT_SOS_9
 | ABOUT_SOS_10
 | ABOUT_SOS_11
 | ABOUT_SOS_12
 | ABOUT_SOS_13
 | THE_VIDEO_WILL_BE_RECORDED
 | EMERGENCY_VIDEO
 | NAMMA_SAFETY_IS_SET_UP
 | PERSONAL_SAFETY_SETTINGS_ACTIONS
 | PERSONAL_SAFETY_SETTINGS_PERMISSION_REQUEST
 | ACTIVATE_NAMMA_SAFETY_POPUP_TITLE
 | ACTIVATE_NAMMA_SAFETY_POPUP_DESC
 | ACTIVATE_NAMMA_SAFETY_POPUP_ACTION
 | DISMISS
 | SEND_SILENT_SOS_TO_POLICE
 | OUR_SAFETY_PARTNER
 | BANGALURU_CITY_POLICE
 | GET_OPTIONS_TO_DIRECTLY_CALL_POLICE
 | SHARE_SOS_SILENTLY_WITH_POLICE
 | SAFETY_PLUS_IS_ACTIVE_NOW
 | SAFETY_PLUS_ACTIVE_DESC
 | CALL_AND_ALERT_THE_NEAREST_POLICE_CENTRE
 | SEND_A_SILENT_SOS_TO_THE_POLICE
 | SEND_A_VIDEO_RECORDING_TO_POLICE
 | NAMMA_SAFETY_WILL_ENABLE_ACCESS_PLUS
 | WHEN_YOU_START_EMERGENCY_SOS_PLUS
 | ABOUT_SOS_DESC_PLUS
 | NAMMA_SAFETY_MEASURES_PLUS
 | ABOUT_SOS_PLUS
 | SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC_PLUS
 | SAFETY_MEASURE_2_PLUS
 | ABOUT_SOS_9_PLUS
 | NAMMA_SAFETY_IS_SET_UP_PLUS
 | ACTIVATE_NAMMA_SAFETY_POPUP_TITLE_PLUS
 | ACTIVATE_NAMMA_SAFETY_POPUP_DESC_PLUS
 | ABOUT_SOS_12_PLUS
 | SAFETY_MEASURE_5_PLUS
 | SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM_PLUS
 | NAMMA_SAFETY_PLUS
 | TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT_PLUS
 | TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE_PLUS
 | PERSONAL_SAFETY_SETTINGS_ACTIONS_PLUS
 | SAFETY_GUIDELINES_6_PLUS
 | ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC_PLUS
 | ALMOST_DONE_DESC_PLUS
 | LEARN_ABOUT_NAMMA_SAFETY_PLUS
 | SEND_VIDEO_TO_POLICE
 | FINISH_SETUP
 | MARK_RIDE_AS_SAFE
 | ACTIVATE_SOS
 | EMERGENCY_INFO_SHARED_ACTION_POLICE
 | START_SETUP
 | CALL_SUPPORT_FOR_SAFETY