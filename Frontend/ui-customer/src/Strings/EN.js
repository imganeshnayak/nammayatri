export function getStringValue(key) {
  if (key in englishStrings) {
    return englishStrings[key];
  }
  console.error(key + " not found in englishStrings");
  return "";
}

const englishStrings = {
  "DOWNLOAD_INVOICE": "Download Invoice",
  "REPORT_AN_ISSUE": "Report an Issue",
  "SUBMIT": "Submit",
  "VIEW_INVOICE": "View Invoice",
  "TOTAL_AMOUNT": "Total Amount",
  "AMOUNT_PAID": "Amount Paid",
  "TRIP_DETAILS_": "Trip Details",
  "DOWNLOAD_PDF": "Download PDF",
  "CGST": "CGST",
  "INVOICE": "Invoice",
  "TRIP_CHARGES": "Trip Charges",
  "PROMOTION": "Promotion",
  "SEND_EMAIL": "Send Email",
  "YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE": "You can describe the issue you faced here",
  "THANK_YOU_FOR_WRITING": "Thank You for writing to us!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE": "We have received your issue. We’ll reach out to you in sometime.",
  "GO_HOME_": "Go Home",
  "LOGO": "Logo",
  "ABOUT_APP_DESCRIPTION": "Namma Yatri is an open platform to connect riders with drivers. The app makes it convenient for riders to book a ride with meter rate hence minimal fare.",
  "ABOUT": "About",
  "PRIVACY_POLICY": "Privacy Policy",
  "SET_UP_YOUR_ACCOUNT": "Set up your account",
  "CONTINUE": "Continue",
  "ENTER_YOUR_NAME": "Enter Your Name",
  "FULL_NAME": "Full Name",
  "EMAIL": "Email",
  "WELCOME_TEXT": "Welcome to Namma Yatri",
  "PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE": "Please choose your preferred language to continue.",
  "WRITE_TO_US": "Write to Us",
  "NOTE": "Note: ",
  "VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS": "Visit My rides section for ride specific complaints",
  "THANK_YOU_FOR_WRITING_TO_US": "Thank You for writing to us!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME": "We have received your issue. We’ll reach out to you in sometime.",
  "GO_TO_HOME__": "Go To Home",
  "SUBJECT": "Subject",
  "YOUR_EMAIL_ID": "Your email ID",
  "DESCRIBE_YOUR_ISSUE": "Describe your Issue",
  "ENTER_MOBILE_NUMBER": "Enter Mobile number",
  "BY_TAPPING_CONTINUE": "By clicking Continue, you agree to our",
  "TO_THE": "You agree that you are accepting the ",
  "ENTER_OTP": "Enter OTP",
  "RESEND": "Resend",
  "ENTER_YOUR_MOBILE_NUMBER": "Enter your Mobile Number ",
  "LOGIN_USING_THE_OTP_SENT_TO": "Login using the OTP sent to",
  "YOUR_RECENT_RIDE": "Your Recent Ride",
  "VIEW_ALL_RIDES": "View All Rides",
  "ALL_TOPICS": "All Topics",
  "FAQ": "FAQ",
  "REPORT_AN_ISSUE_WITH_THIS_TRIP": "Report an issue with this Trip",
  "YOU_RATED": "You Rated:",
  "GETTING_STARTED_AND_FAQS": "Getting started and FAQs",
  "FOR_OTHER_ISSUES_WRITE_TO_US": "For other issues, write to us",
  "HELP_AND_SUPPORT": "Help and Support",
  "OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS": "Our suggested price for this trip is",
  "DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE": "*Drivers can charge between the above range",
  "HOW_THIS_WORKS": "How this works?",
  "FINDING_RIDES_NEAR_YOU": "Finding rides near you...",
  "CONFIRMING_THE_RIDE_FOR_YOU": "Confirming the ride for you...",
  "CANCEL_SEARCH": "Cancel Search",
  "YOUR_RIDE_IS_NOW_COMPLETE": "Your Ride is now complete!",
  "PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH": "Please pay the final amount to the driver directly",
  "WHERE_TO": "Where to?",
  "HOME": "Home",
  "PICK_UP_LOCATION": "Pick Up Location",
  "REQUEST_RIDE": "Request Ride",
  "NAME": "Name",
  "MOBILE_NUMBER_STR": "Mobile Number",
  "PERSONAL_DETAILS": "Personal Details",
  "YOUR_RIDES": "Your Rides",
  "YOU_ARE_OFFLINE": "You're Offline",
  "CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN": "Check your internet connection and try again",
  "TRY_AGAIN": "Try again",
  "THANK_YOUR_DRIVER": "Thank your Driver!",
  "HOPE_YOUR_RIDE_WAS_HASSLE_FREE": "We hope your ride was hassle free",
  "HOW_WAS_YOUR_RIDE_WITH": "How was your ride with ",
  "GOT_IT_TELL_US_MORE": "Got it, Tell us more?",
  "WRITE_A_COMMENT": "Write a comment (Optional)",
  "UPDATE": "Update",
  "LANGUAGE": "Language",
  "OTP": "OTP",
  "PAYMENT_METHOD": "Payment Method",
  "PAYMENT_METHOD_STRING": "Cash / Use UPI app",
  "PAYMENT_METHOD_STRING_": "Cash / Use UPI app",
  "CANCEL_RIDE": "Cancel Ride",
  "SUPPORT": "Support",
  "PICKUP_AND_DROP": "Pickup and Drop",
  "CANCELLED": "Cancelled",
  "HOW_THE_PRICING_WORKS": "How the pricing works?",
  "SELECT_AN_OFFER": "Select an Offer",
  "CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT": "Choose a ride as per your comfort ",
  "IT_SEEMS_TO_BE_A_VERY_BUSY_DAY": "It seems to be a very busy day. You may try looking for rides again",
  "SORT_BY": "Sort By",
  "SORRY_WE_COULDNT_FIND_ANY_RIDES": "Sorry, we couldn’t find any rides",
  "LOAD_MORE": "Load More",
  "WE_NEED_ACCESS_TO_YOUR_LOCATION": "We need access to your location!",
  "LOCATION_PERMISSION_SUBTITLE" : "To get you rides, we require your device location.",
  "CALL": "Call",
  "EMPTY_RIDES": "Empty Rides",
  "YOU_HAVENT_TAKEN_A_TRIP_YET": "You haven't taken a trip yet",
  "BOOK_NOW": "Book Now",
  "T_AND_C_A": "a) You agree that you are a willing participant of the beta testing and Juspay shall have no liability against you in any respect",
  "TERMS_AND_CONDITIONS": "T&C",
  "DATA_COLLECTION_AUTHORITY": "c) I hereby appoint and authorize Juspay to collect my information and by continuing, I agree to the Terms of Use and Privacy Policy.",
  "DENY_ACCESS": "Deny Access",
  "PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL": "Please tell us why you want to cancel",
  "MANDATORY": "Mandatory",
  "SOFTWARE_LICENSE": "Software license",
  "LOGOUT_": "Logout",
  "REQUEST_AUTO_RIDE": "Request Auto Ride",
  "RATE_YOUR_RIDE": "Rate your ride",
  "SKIP": "Skip",
  "ERROR_404": "Error 404",
  "PROBLEM_AT_OUR_END": "There seems to be a problem at our end. Get notified when we are up again",
  "NOTIFY_ME": "Notify me",
  "ADDRESS": "Address",
  "CHANGE": "Change",
  "SAVE_AS": "Save as",
  "ADD_TAG": "Add Tag",
  "WORK": "Work",
  "OTHER": "Other",
  "SAVE": "Save",
  "ADD_NEW_ADDRESS": "Add New Address",
  "SAVED_ADDRESSES": "Saved Addresses",
  "ADDRESSES": "Addresses",
  "NO_FAVOURITES_SAVED_YET": "No favourites saved yet",
  "SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY": "Favourite location helps in keeping your frequently visited places handy",
  "EMERGENCY_CONTACTS": "Emergency Contacts",
  "ADD_EMERGENCY_CONTACTS": "Add Emergency Contacts",
  "ADD_ANOTHER_CONTACT": "Add Another Contact",
  "NO_EMERGENCY_CONTACTS_SET": "No Emergency Contacts Set",
  "EMERGENCY_CONTACTS_SCREEN_DESCRIPTION": "You can share your ride status in case of an emergency with upto 3 emergency contacts.",
  "COPIED": "Copied",
  "TRIP_ID": "Trip Id",
  "SAVE_PLACE": "Save Place",
  "RIDE_FARE": "Ride Fare",
  "ASK_FOR_PRICE": "Ask for a price",
  "ASK_FOR_PRICE_INFO": "You will receive a fare based on <b> government set </b> base price , with an additional <b> Rs.10 </b> nominal fee for the <b> pick-up distance </b> traveled by the drivers. Some drivers may request nominal tips solely at their discretion to cover factors like traffic, chances of return trip etc.",
  "GET_ESTIMATE_FARE": "Get estimate fare",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS": "Select an offer (Optional)",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO": "By default, when “<b>Auto-assign a ride</b>” is enabled, you are assigned a driver who accepts the request first, within the estimate range. Instead, if you wish to choose a driver offer, you can disable and proceed.",
  "PAY_THE_DRIVER": "Pay the driver",
  "PAY_THE_DRIVER_INFO": "Pay the driver directly, the price that was confirmed by you",
  "PAY_THE_DRIVER_NOTE": "(Total fare may change if ride distance changes)",
  "UPDATE_PERSONAL_DETAILS": "Update Personal Details",
  "EDIT": "Edit",
  "DEL_ACCOUNT": "Delete Account",
  "ACCOUNT_DELETION_CONFIRMATION": "Are you sure you want to delete your account? All your personal data will be lost",
  "REQUEST_SUBMITTED": "Request Submitted",
  "WE_WILL_DELETE_YOUR_ACCOUNT": "We are sorry to see you leave our platform. Your account will be deleted within the next 30 days. Meanwhile if you wish to retain your account, please call to our customer support number",
  "YES_DELETE_IT": "Yes, Delete It",
  "REQUEST_TO_DELETE_ACCOUNT": "Request to delete account",
  "CANCEL_STR": "Cancel",
  "LOADING": "Loading",
  "PLEASE_WAIT_WHILE_IN_PROGRESS": "Please wait while in progress",
  "SET_LOCATION_ON_MAP": "Set location on map",
  "CURRENT_LOCATION": "Current Location",
  "ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN": "Actual fare was higher than what was shown",
  "DELETE": "Delete",
  "ARE_YOU_SURE_YOU_WANT_TO_LOGOUT": "Are you sure you want to logout?",
  "ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "Are you sure you want to Cancel?",
  "YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "You have ride offers, are you sure you want to cancel?",
  "GO_BACK_": "Go Back",
  "REGISTER_USING_DIFFERENT_NUMBER": "Do you want to register using a different mobile number?",
  "YES": "Yes",
  "NO": "No",
  "CANCEL_": "Cancel",
  "IS_ON_THE_WAY": "is on the way..",
  "ENTER_4_DIGIT_OTP": "Enter 4 digit OTP",
  "WRONG_OTP": "Wrong OTP",
  "GRANT_ACCESS": "Grant Location Access",
  "ENTER_A_LOCATION": "Enter a location",
  "NEARBY": "Nearby",
  "MINS_AWAY": "mins Away",
  "PAID": "Paid",
  "BY_CASH": "by Cash",
  "ONLINE_": "Online",
  "USER": "User",
  "EMAIL_ALREADY_EXISTS": "Failed to update email. Email already exists.",
  "IN": "in",
  "VERIFYING_OTP": "Verifying OTP",
  "TRACK_LIVE_LOCATION_USING": "Track live location using",
  "GOOGLE_MAP_": "Google Map",
  "IN_APP_TRACKING": "In app tracking",
  "LIMIT_EXCEEDED": "Limit Exceeded",
  "QUOTE_EXPIRED": "Quote Expired",
  "GETTING_ESTIMATES_FOR_YOU": "Getting estimates for you...",
  "CONFIRM_PICKUP_LOCATION": "Confirm Pickup Location",
  "CONFIRM_DROP_LOCATION": "Confirm Drop Location",
  "ERROR_OCCURED_TRY_AGAIN": "Error Occured. Try Again",
  "ASKED_FOR_MORE_MONEY": "Asked for more money",
  "START_": "Start",
  "RIDE_NOT_SERVICEABLE": "Ride Not Serviceable",
  "APP_NOT_SERVICEABLE": "Not Serviceable",
  "CONFIRM_FOR": "Confirm for",
  "ETA_WAS_TOO_SHORT": "ETA was too short.",
  "DRIVER_REQUESTED_TO_CANCEL": "Driver requested me to cancel",
  "PICK_UP_LOCATION_INCORRECT": "The pickup location was incorrect.",
  "COULD_NOT_CONNECT_TO_DRIVER": "I could not connect to driver.",
  "ETA_WAS_TOO_LONG": "ETA was too long.",
  "OTHERS": "Others",
  "DESTINATION_OUTSIDE_LIMITS": "Destination Entered is outside city limits",
  "DROP_LOCATION_FAR_AWAY": "Your drop location is too far away",
  "CHANGE_DROP_LOCATION": "Change Drop Location",
  "YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING": "You may wish to walk or continue with the ride booking",
  "YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST": "Your trip distance is too short. Destination is just ",
  "METERS_AWAY_FROM_YOUR_DESTINATION": "m away!",
  "BOOK_RIDE_": "Book Ride",
  "CANCEL_AUTO_ASSIGNING": "Cancel Auto Assigning",
  "LOCATION_UNSERVICEABLE": "Location unserviceable",
  "CURRENTLY_WE_ARE_LIVE_IN_": "We are not live in your area yet!\nYou can access ride history and other settings from the menu",
  "CHANGE_LOCATION": "Change Location",
  "AUTO_ACCEPTING_SELECTED_RIDE": "Auto accepting in",
  "THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE": "The trip is very short and just take",
  "IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE": "if you still want to book, click continue and start booking the ride",
  "STEPS_TO_COMPLETE": "Steps to complete",
  "HELP_US_WITH_YOUR_REASON": "Help us with your reason",
  "MAX_CHAR_LIMIT_REACHED": "Max character limit reached,",
  "DRIVER_WAS_NOT_REACHABLE": "Driver wasn't reachable",
  "SHOW_ALL_OPTIONS": "Show all options",
  "EXPIRES_IN": "Expires in",
  "PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI": "*Pay directly to your driver using Cash / UPI",
  "UPDATE_REQUIRED": "Update Required",
  "PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE": "We're excited to announce that there's a new update available for our app. This update includes a fresh new design and several new features to make your experience even better.",
  "NOT_NOW": "Not Now",
  "OF": "of",
  "LOST_SOMETHING": "Lost Something?",
  "TRY_CONNECTING_WITH_THE_DRIVER": "You can request a callback to connect with the driver directly",
  "CALL_DRIVER": "Call Driver",
  "NO_MORE_RIDES": "No more rides",
  "CONTACT_SUPPORT": "Contact Support",
  "INVALID_MOBILE_NUMBER": "Invalid mobile number",
  "RIDE_COMPLETED": "Ride Completed",
  "SUBMIT_FEEDBACK": "Submit Feedback",
  "HOW_WAS_YOUR_RIDE_EXPERIENCE": "How was your ride experience?",
  "DROP": "Drop",
  "RATE_YOUR_RIDE_WITH": "Rate your ride with ",
  "VIEW_BREAKDOWN": "View Breakdown",
  "PAY_DRIVER_USING_CASH_OR_UPI": "Pay Driver using Cash/UPI",
  "PAY_DRIVER_USING_CASH_OR_UPI_": "Pay Driver using Cash/UPI",
  "RATE_YOUR_DRIVER": "Rate Your Driver",
  "MY_RIDES": "My Rides",
  "RIDE_DETAILS": "Ride Details",
  "RIDE_ID": "Ride ID",
  "SELECT_A_RIDE": "Select a ride for more details",
  "CONFIRM_RIDE_": "Confirm Ride",
  "YOU_CAN_CANCEL_RIDE": "You can cancel once you get offers from drivers",
  "ESTIMATES_CHANGED": "The estimate for your ride has changed now",
  "ESTIMATES_REVISED_TO": "The revised estimate is",
  "RATE_CARD": "Rate Card",
  "NIGHT_TIME_CHARGES": "Night-time Charges (10 PM to 5 AM)",
  "MIN_FARE_UPTO": "Min. Fare upto",
  "MIN_FARE_UPTO_4_KM": "Min. Fare upto 4 km",
  "MORE_THAN" : "More than",
  "RATE_ABOVE_MIN_FARE": "Rate above Min. Fare",
  "DRIVER_PICKUP_CHARGES": "Driver Pickup Charges",
  "DAY_TIMES_OF": "",
  "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT": "x of daytime charges applicable at night from 10 PM to 5 AM",
  "NIGHT_TIMES_OF": "",
  "DAYTIME_CHARGES_APPLIED_AT_NIGHT": "x of daytime charges applied to fare at night (🌙) from 10 PM to 5 AM",
  "DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "* Driver may optionally request 10% of base fare (rounded to nearest Rs.10) to cover for traffic, chance of return trip etc.",
  "GOT_IT": "Got It!",
  "DAY_TIME_CHARGES": "Daytime Charges (5 AM to 10 PM)",
  "SHARE_APP": "Share App",
  "AWAY_C": "Away",
  "AWAY": "away",
  "AT_PICKUP": "At Pickup",
  "FARE_UPDATED": "Fare updated",
  "TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE": "Total fare may change due to change in route",
  "AT_DROP": "At Drop",
  "EMERGENCY_HELP": "Emergency Help",
  "CALL_POLICE": "Call Police",
  "ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION": "Also shares your ride status and location",
  "SHARE_RIDE_WITH_EMERGENCY_CONTACTS": "Share Ride with Emergency Contacts",
  "DO_YOU_NEED_EMERGENCY_HELP": "Do you need Emergency Help ?",
  "CALL_SUPPORT": "Call Support",
  "YOU_ARE_ABOUT_TO_CALL_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Namma Yatri Support Team. Do you want to proceed?",
  "YOU_ARE_ABOUT_TO_CALL_NEAREST_EMERGENCY_CENTRE": "You are about to place a call to the nearest Emergency Centre. Do you want to proceed?",
  "DIAL_112": "Dial 112",
  "HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL": "Help us with your feedback (Optional)",
  "WAIT_TIME": "Wait Time",
  "FAVOURITES": "Favourites",
  "ADD_FAVOURITE": "Add Favourite",
  "ALL_FAVOURITES": "All Favourites",
  "REMOVE": "Remove",
  "CONFIRM_LOCATION": "Confirm Location",
  "SELECT_ON_MAP": "Select on map",
  "FAVOURITE_LOCATION": "Favourite Location",
  "EDIT_FAVOURITE": "Edit favourite",
  "DRAG_THE_MAP": "Drag the map & set pin to exact location",
  "CHOOSE_ON_MAP": "Choose on map",
  "USE_CURRENT_LOCATION": "Use current location",
  "FAVOURITE_YOUR_CURRENT_LOCATION": "Favourite your current location",
  "LOCATION": "Location",
  "LOCATION_ALREADY_EXISTS_AS": "Location already exists as",
  "GIVE_THIS_LOCATION_A_NAME": "Give this location a name",
  "FAVOURITE": "Favourite",
  "CONFIRM_AND_SAVE": "Confirm & Save",
  "REMOVE_FAVOURITE": "Remove Favourite",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_": "Are you sure you want to remove a \n favourite location?",
  "YES_REMOVE": "Yes, Remove",
  "ADD_NEW_FAVOURITE": "Add New Favourite",
  "SELECT_YOUR_DROP": "Select your drop",
  "FAVOURITE_REMOVED_SUCCESSFULLY": "Favourite Removed Successfully",
  "LOCATION_ALREADY_EXISTS": "location already exists",
  "LOCATION_ALREADY": "Location already",
  "EXISTS_AS": "exists as",
  "FAVOURITE_ADDED_SUCCESSFULLY": "Favourite Added Successfully",
  "FAVOURITE_UPDATED_SUCCESSFULLY": "Favourite Updated Successfully",
  "ALREADY_EXISTS": "Already Exists",
  "NAME_ALREADY_IN_USE": "Name already in-use",
  "SELECT_FAVOURITE": "Select Favourite",
  "CONFIRM_CHANGES": "Confirm Changes",
  "ADD_SAVED_LOCATION_FROM_SETTINGS": "*You can add new favourites from Side menu > Favourites",
  "YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS": "You will be asked to select upto 3  contacts",
  "AUTO_ASSIGN_A_RIDE": "Auto-assign a ride",
  "IS_WAITING_FOR_YOU": "is waiting for you...",
  "WAIT_TIME_TOO_LONG": "Wait time too long",
  "GOT_ANOTHER_RIDE_ELSE_WHERE": "Got another ride elsewhere",
  "DRIVER_WAS_RUDE": "Driver was rude",
  "MAYBE_LATER": "Maybe Later",
  "YOUR_RIDE_HAS_STARTED": "Yay! Your ride has started 🤩",
  "ENJOY_RIDING_WITH_US": "Enjoying riding with us? Spread the \n word and share the cheer",
  "VIEW_DETAILS": "View Details",
  "REPEAT_RIDE": "Repeat Ride",
  "FARE_WAS_HIGH": "Fare was high",
  "AUTO_ASSIGN_DRIVER": "Auto-assign a driver",
  "CHOOSE_BETWEEN_MULTIPLE_DRIVERS": "Choose between multiple drivers",
  "CHOOSE_BETWEEN_MULTIPLE_RIDES": "Choose between multiple ride options",
  "ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE": "Enable this feature to choose your preferred ride & fare",
  "BOOKING_PREFERENCE": "Booking Preference",
  "BASE_FARES": "Base Fare ",
  "PICKUP_CHARGE": "Driver Pickup Charges",
  "TOTAL_PAID": "Total Paid",
  "WAITING_CHARGE": "Waiting Charges**",
  "NOMINAL_FARE": "Optional Driver Request*",
  "DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO": "* Drivers may optionally request 10% of base fare (rounded to nearest Rs.10) to cover other factors like traffic, chances of return trip etc.",
  "WAITING_CHARGE_DESCRIPTION": "** Waiting charges : ₹1 / min - after 3 mins of drivers arrival",
  "SUCCESSFUL_ONBOARD": "You have successfully signed on to \nNamma Yatri",
  "HAVE_REFERRAL_CODE": "Have a Referral Code?",
  "REFEREAL_CODE_DISCRIPTION": "Your referral might just be the reason for \n a driver to be rewarded!",
  "SIX_DIGIT_REFERRAL_CODE": "Enter 6 digit code shared by driver",
  "ABOUT_REFERRAL_PROGRAM": "What is the Referral Program?",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Namma Yatri Community!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Namma Yatri Driver.",
  "REFERRAL_CODE_SUCCESSFULL": "You have successfully applied \nthe referral code!",
  "REFERRAL_CODE_APPLIED": "Referral Applied!",
  "HEY": "Hey",
  "INVALID_CODE_PLEASE_RE_ENTER": "Invalid Code. Please Re-enter",
  "LET_TRY_THAT_AGAIN": "Let's try that again...",
  "CONTACTS_SELECTED": "Contacts Selected",
  "SELECT_CONTACTS": "Select Contacts",
  "CONFIRM_EMERGENCY_CONTACTS": "Confirm Emergency Contacts",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT": "Are you sure you want to remove them from your emergency contacts?",
  "SEARCH_CONTACTS": "Search Contacts",
  "CALL_EMERGENCY_CONTACTS": "Call Emergency Contacts",
  "LIVE_STATS_DASHBOARD": "Live Stats Dashboard",
  "CHECK_OUT_LIVE_STATS": "Check out live stats",
  "EMERGENCY_CONTACS_ADDED_SUCCESSFULLY": "Emergency Contacts Added Successfully",
  "NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD": "No contacts left on device to add",
  "PERCENTAGE_OF_NOMINAL_FARE": "10% of base fare",
  "PAY_VIA_CASH_OR_UPI": "Pay via Cash / UPI",
  "REQUEST_CALLBACK": "Request Callback",
  "CHOOSE_YOUR_RIDE": "Choose Your Ride",
  "BOARD_THE_FIRST": "Board the first ",
  "TAXI" : "taxi" ,
  "AC" : "AC",
  "NON_AC" : "Non-AC",
  "TAXI_FROM_ZONE" : "from Yatri Sathi Zone",
  "ECONOMICAL": "Economical",
  "COMFY": "Comfy",
  "NAVIGATE": "Navigate",
  "GOVERNMENT_CHAGRES": "Ride GST (5%)",
  "SERVICE_CHARGES": "Service Charges",
  "CONFIRM_AND_BOOK": "Confirm & Book",
  "PEOPLE": "people",
  "SPACIOUS": "Spacious",
  "EASY_ON_WALLET": "Easy on wallet",
  "UPTO": "upto",
  "CANCEL_ONGOING_SEARCH": "Are you sure you want to continue with cancelling an ongoing search?",
  "SEARCH_AGAIN_WITH_A_TIP": "Search again with a tip?",
  "TRY_AGAIN_WITH_A_TIP": "Try again with a tip?",
  "BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS": "It seems to be a very busy day. You may try adding a tip to increase chances of getting a ride.",
  "TRY_AGAIN_WITHOUT_TIP": "Try Again Without Tip",
  "SEARCH_AGAIN_WITHOUT_A_TIP": "Search Again Without Tip",
  "TRY_AGAIN_WITH": "Try Again With",
  "SEARCH_AGAIN_WITH": "Search Again With",
  "TIP": "Tip",
  "CUSTOMER_SELECTED_FARE": "Customer Tip^",
  "START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS": "Start your chat using these quick chat suggestions",
  "MESSAGE": "Message",
  "START_YOUR_CHAT_WITH_THE_DRIVER": "Start your chat with the driver",
  "I_AM_ON_MY_WAY": "I'm on my way",
  "GETTING_DELAYED_PLEASE_WAIT": "Getting delayed, please wait",
  "UNREACHABLE_PLEASE_CALL_BACK": "Unreachable, Please call back",
  "ARE_YOU_STARING": "Are you starting?",
  "PLEASE_COME_SOON": "Please come soon",
  "OK_I_WILL_WAIT": "Ok, I'll wait",
  "I_HAVE_ARRIVED": "I've arrived",
  "PLEASE_COME_FAST_I_AM_WAITING": "Please come fast, I'm waiting",
  "PLEASE_WAIT_I_WILL_BE_THERE": "Please wait, I'll be there",
  "LOOKING_FOR_YOU_AT_PICKUP": "Looking for you at pick-up",
  "MOBILE": "Mobile",
  "HOW_DO_YOU_IDENTIFY_YOURSELF": "How do you identify yourself as?",
  "SELECT_YOUR_GENDER": "Select Your Gender",
  "FEMALE": "Female",
  "MALE": "Male",
  "PREFER_NOT_TO_SAY": "Prefer not to say",
  "EMAIL_ID": "Email ID",
  "SET_NOW": "Set now",
  "ADD_NOW": "Add now",
  "HOW_SHOULD_WE_ADDRESS_YOU": "How should we address you?",
  "GENDER_STR": "Gender",
  "PROFILE_COMPLETION": "Profile Completion",
  "EARLY_END_RIDE_CHARGES": "Early Ride End Charges^",
  "EARLY_END_RIDE_CHARGES_DESCRIPTION": "^Ending a ride early incurs additional charges amounting to half the fare of the untravelled distance (max. ₹50)",
  "YES_TRY_AGAIN": "Yes, Try Again",
  "NO_DONT": "No, Don’t",
  "YES_CANCEL_SEARCH": "Yes, Cancel Search",
  "TRY_LOOKING_FOR_RIDES_AGAIN": "It seems to be a very busy day. You may try looking for rides again",
  "NO_TIP": "₹0 Tip",
  "CUSTOMER_TIP_DESCRIPTION": "^Extra amount added by the customer to increase the chances of getting a ride.",
  "PLEASE_WAIT_I_WILL_BE_THERE": "Please wait, I'll be there",
  "UNREACHABLE_PLEASE_CALL_BACK": "Unreachable, Please call back",
  "PLACE_CALL": "Place Call",
  "DIRECT_CALL": "Direct Call",
  "ANONYMOUS_CALL": "Anonymous Call",
  "YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE": "Your number will not be shown to the driver. The call will be recorded for compliance.",
  "YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER": "Your number will be visible to the driver. Use if not calling from registered number.",
  "CALL_DRIVER_USING": "Call Driver using",
  "WAS_YOUR_CALL_SUCCESSFUL": "Was Your Call Successful",
  "DRIVER_ADDITIONS": "Driver Additions*",
  "FARE_UPDATE_POLICY": "Fare Update Policy",
  "DRIVER_ADDITIONS_OPTIONAL" : "Driver Additions (Optional)",
  "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC" : "The Driver may quote extra to cover for traffic, chance of return trip etc.",
  "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE" : "Driver addition limits are calculated at 10% of the base fare rounded off to the nearest ₹10",
  "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE" : "Note: The driver may/may not charge this additional fare",
  "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS" : "You may see an updated final fare due to any of the below reasons:",
  "REASON_CHANGE_IN_ROUTE_A": "1.Change in Route: ",
  "REASON_CHANGE_IN_ROUTE_B": "Total fare may change due to change in route",
  "GO_TO_ZONE" : "Go to Yatri Sathi Zone",
  "UPDATE_NOW" : "Update Now",
  "REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON" : "Request received. We will call you back soon",
  "CONTACT_REMOVED_SUCCESSFULLY" : "Contact Removed Successfully",
  "CORPORATE_ADDRESS" : "Corporate Address",
  "CORPORATE_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Girija Building, Number 817, Ganapathi Temple Rd, 8th Block, Koramangala, Bengaluru, Karnataka 560095, India.",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" :  "Website: <u>https://nammayatri.in/</u>",
  "REGISTERED_ADDRESS" : "Registered Address",
  "REGISTERED_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala Bengaluru, Karnataka- 560095, India.",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
  "RECOMMENDED" : "Recommended",
  "COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE" : "Complete your profile for a personalised ride experience",
  "WE_WOULD_APPRECIATE_YOUR_FEEDBACK" : "We would appreciate your reasoning behind account deletion",
  "REASON_FOR_DELETING_ACCOUNT" : "Reason for deleting account*",
  "SUBMIT_REQUEST" : "Submit Request",
  "PLEASE_ENTER_A_VALID_EMAIL" : "Please enter a valid email",
  "WE_WOULD_APPRECIATE_YOUR_REASONING" : "We would appreciate your feedback on your reasoning behind account deletion",
  "OK_GOT_IT" : "Ok, Got it",
  "WAIT_FOR_DRIVER" : "Wait For Driver",
  "NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS" : "No longer require a ride due to change in plans",
  "CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP" : "Cancelling as I got a ride on another app",
  "DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP" : "Driver location wasn`t changing on the map",
  "DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION" : "Driver was taking too long to reach the pickup location",
  "THE_PICKUP_LOCATION_ENTERED_WAS_WRONG" : "The pickup location entered was wrong",
  "YOUR_DRIVER_IS_JUST" : "Your Driver is just ",
  "M_AWAY" : " m away.",
  "DRIVER_HAS_ALREADY_TRAVELLED" : "Driver has already travelled ",
  "PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING" : "\nPlease contact the driver before cancelling",
  "CHANGE_OF_PLANS" : "Change of plans",
  "DRIVER_IS_NOT_MOVING" : "Driver is not moving",
  "WRONG_PICKUP_LOCATION" : "Wrong pickup location",
  "DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE" : "Driver might be taking an alternate route.",
  "DRIVER_IS_NOT_MOVING_Q" : "Driver is not moving?",
  "WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING" : "\nWould you like to check with the driver before cancelling?",
  "DRIVER_IS_NEAR_YOUR_LOCATION" : "Driver is near your location.",
  "SOME_OTHER_REASON" : "Some other reason.",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Namma Yatri! \nTo start booking rides, we require your device location.",
  "METRO_RIDE" : "Metro Ride",
  "GO_BACK_TEXT" : "Go Back",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST": "The driver preferred your special request and is just ",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST": "The driver preferred your special request and has travelled ",
  "AND_HAS_TRAVELLED": ".",
  "PLEASE_FIND_REVISED_FARE_ESTIMATE": "Please find the Revised Estimated Fare. Night charges are 1.5 times the day charges.",
  "FARE_ESTIMATE" : "Fare Estimate",
  "TIP_SELECTED" : "Tip Selected",
  "ADD_A_TIP_TO_FIND_A_RIDE_QUICKER" : "Add a tip to find a ride quicker!",
  "IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL" : "It seems to be taking longer than usual.",
  "CONTINUE_SEARCH_WITH" : "Continue Search with",
  "CONTINUING_SEARCH_WITH" : "Continuing search with",
  "SEARCHING_WITH" : "Searching with",
  "THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "The driver preferred your special request and is already on the way to your location.",
  "DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "Driver is already on the way to your location.",
  "ALLOW_LOCATION_ACCESS": "Allow Location Access",
  "MESSAGE_FROM_DRIVER": "Message from Driver",
  "REPLY": "Reply",
  "NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS" : "Name should be more than 2 characters",
  "THIS_FIELD_IS_REQUIRED" : "This field is required",
  "EMAIL_EXISTS_ALREADY" : "Email already exists",
  "OKAY_GOT_IT" : "Ok, Got it",
  "CALL_NAMMA_YATRI_SUPPORT" : "Call Namma Yatri Support",
  "CALL_112": "Call 112",
  "CALL_EMERGENCY_CENTRE": "Call Emergency Centre",
  "SEATS" : "Seats",
  "DRIVER_ADDITION_LIMITS_ARE_IN_INCREMENTS" : "Driver addition limits are in increments of ₹10",
  "HATCHBACK" : "Hatchback",
  "SUV" : "SUV",
  "SEDAN" : "Sedan",
  "OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN" : "OTP page has been expired, please request OTP again",
  "OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER" : "OTP entering limit exhausted, please try again later",
  "TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER" : "Too many login attempts, please try again later",
  "SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN" : "Something went wrong, please try again",
  "SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES" : "Sorry, limit exceeded. You can't add any more favourites",
  "IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_BOOKING_PLEASE_RESTART_THE_APPTO_VIEW_IT" : "It seems that you have an on-going booking, please restart the app to view it. ",
  "CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN" : "Cancellation unsuccessfull, please try again",
  "NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN" : "No drivers available at the moment, please try again",
  "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "OTP for the Jatri Sathi zone has been expired, please try booking again",
  "NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED" : "No contacts found on the device to be added",
  "PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED" : "Please enable contacts permission to proceed",
  "LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED" : "Limit reached! 3 of 3 Emergency contacts already added",
  "INVALID_CONTACT_FORMAT" : "Invalid contact format",
  "OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER" : "OTP resend limit exhausted, please try again later",
  "RATE_YOUR_EXPERIENCE" : "Rate your experience ✨",
  "REPORT_ISSUE_" : "Report Issue",
  "DONE" : "Done",
  "PLEASE_TELL_US_WHAT_WENT_WRONG" : "Please tell us what went wrong",
  "DID_YOU_FACE_ANY_ISSUE" : "Did you face any issue?",
  "WE_NOTICED_YOUR_RIDE_ENDED_AWAY" : "We noticed  that your ride ended away from your original destination.",
  "GET_CALLBACK_FROM_US" : "Get a callback from us",
  "DRIVER_WAS_NOT_READY_TO_GO" : "The driver was not to ready to go",
  "ASKING_FOR_MORE_MONEY" : "Driver was asking for more money",
  "AUTO_BROKEN" : "Auto broke down",
  "WE_WILL_GIVE_YOU_CALLBACK" : "We will give you a callback within 24 hrs",
  "YOUR_ISSUE_HAS_BEEN_REPORTED" : "Your issue has been reported successfully",
  "OTP_RESENT_SUCCESSFULLY" : "OTP resent successfully",
  "DESCRIPTION_SHOULD_BE_MORE_THAN_10_ALPHABETIC_CHARACTERS" : "Description should be more than 10 alphabetic characters",
  "IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_" : "It seems that you have an on-going booking, please restart the app to view it",
  "INCORRECT_OTP_PLEASE_TRY_AGAIN" : "Incorrect OTP, please try again.",
  "N_MORE_ATTEMPTS_LEFT" : " more attempts left",
  "GO_TO_SELECTED_PICKUP_SPOT" : "Go to selected pickup spot",
  "GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED" : "Go to the suggested pickup spot as Autos are restricted inside this area",
  "UNPROFESSIONAL_DRIVER": "Unprofessional Driver",
  "RASH_DRIVING": "Rash Driving",
  "DRIVER_CHARGED_MORE": "Driver charged more",
  "UNCOMFORTABLE_AUTO": "Uncomfortable Auto",
  "TRIP_GOT_DELAYED": "Trip Got Delayed",
  "FELT_UNSAFE": "Felt Unsafe",
  "POLITE_DRIVER": "Polite Driver",
  "EXPERT_DRIVING": "Expert Driving",
  "SAFE_RIDE": "Safe Ride",
  "CLEAN_AUTO": "Clean Auto",
  "ON_TIME": "On Time",
  "SKILLED_NAVIGATOR": "Skilled Navigator",
  "RUDE_DRIVER": "Rude Driver",
  "TOO_MANY_CALLS": "Too Many Calls",
  "RECKLESS_DRIVING": "Reckless Driving",
  "LATE_DROP_OFF": "Late Drop Off",
  "LATE_PICK_UP": "Late Pick Up",
  "POOR_EXPERIENCE": "Poor Experience 😕",
  "TERRIBLE_EXPERIENCE": "Terrible Experience 😠",
  "NEEDS_IMPROVEMENT": "Needs Improvement 😕",
  "ALMOST_PERFECT": "Almost Perfect! 🙂",
  "AMAZING": "Amazing!!! 🤩",
  "ASKED_FOR_EXTRA_FARE":  "Asked for Extra Fare",
  "ANYTHING_THAT_YOU_WOULD_LIKE_TO_TELL_US" : "Anything that you would like to tell us? (Optional)",
  "PLEASE_WAIT" : "Please wait",
  "PLATFORM_FEE" : "Platform Fee",
  "FINDING_QUOTES_TEXT": "Searching for cabs around",
  "FASTER" : "Faster",
  "NEW_" : "New",
  "PLATFORM_FEE" : "Platform Fee",
  "SGST" : "SGST",
  "OTP_EXPIRED" : "Otp Expired",
  "OTP_EXPIRED_DESCRIPTION" : "Your ride OTP expired. Please book again to get a ride",
  "PLATFORM_GST" : "Taxes (GST)",
  "MISC_WAITING_CHARGE" : "Misc. (including Waiting Charges)",
  "AC_TAXI" : "AC Taxi",
  "NON_AC_TAXI" : "Non AC Taxi",
  "GET_OTP_VIA_WHATSAPP" : "Get OTP via WhatsApp",
  "OR" : "Or",
  "HELPS_DRIVER_CONFIRM_ITS_YOU" : "(Helps driver confirm it is you)",
  "LETS_GET_YOU_TRIP_READY" : "Let’s get you trip-ready!",
  "GOT_AN_OTP" : "Got an OTP?",
  "JUST_ONE_LAST_THING" : "Just one last thing",
  "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri Sathi experience",
"TOLL_CHARGES_WILL_BE_EXTRA" : "Toll/Parking charges will be additional",
  "AUTO_RICKSHAW" : "Auto Rickshaw",
  "CABS_AVAILABLE" : "cabs available",
  "PI_POINTER_1" : "Drivers will be prompted to come to your exact location for pickup.",
  "PI_POINTER_2" : "Drivers will be prompted to help you with your mobility aid.",
  "VI_POINTER_1" : "Drivers will be prompted to call instead of texting.",
  "VI_POINTER_2" : "Drivers will be prompted to sound horn once at the pickup.",
  "HI_POINTER_1" : "Drivers will be prompted to text instead of calling.",
  "HI_POINTER_2" : " Drivers will be prompted to message you once at the pickup.",
  "GENERAL_DISABILITY_DESCRIPTION" : "Drivers will be sensitised to help you with your needs and requests.",
  "ACCESSIBILITY_TEXT" : "Namma Yatri, now customised for you!",
  "TO_CATER_YOUR_SPECIFIC_NEEDS" : "To cater to your specific needs, we have customised certain features of Namma Yatri.",
  "SPECIAL_ASSISTANCE" : "Special Assistance",
  "SELECT_THE_CONDITION_THAT_IS_APPLICABLE" : "Select the condition that is applicable to you\n(As per RPWD act 2016)",
  "DISABILITY_CLAIMER_TEXT" : "By continuing, you are declaring your status as a person with disability under the RPWD act of 2016",
  "ARE_YOU_A_PERSON_WITH_DISABILITY" : "Are you a person with disability?",
  "DO_YOU_NEEED_SPECIAL_ASSISTANCE" : "Do you require special assitance (Person With Disability)?",
  "ASSISTANCE_REQUIRED" : "Assistance Required (Person with Disability)",
  "NO_DISABILITY" : "No Disability",
  "LEARN_HOW_TEXT" : "Learn how Namma Yatri caters to your needs",
  "UPDATE_PROFILE" : "Update Profile",
  "NOW_GET_ASSISTED_RIDES" : "Now get assisted rides if you are a person with disability" ,
  "CLEAN_CAB" : "Clean Cab"
}
