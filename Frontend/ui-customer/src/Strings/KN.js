export function getStringValue(key) {
  if (key in kannadaStrings) {
    return kannadaStrings[key];
  }
  console.error(key + " not found in kannadaString");
  return "";
}

const kannadaStrings = {
  "DOWNLOAD_INVOICE": "ಸರಕುಪಟ್ಟಿ ಡೌನ್‌ಲೋಡ್ ಮಾಡಿ",
  "REPORT_AN_ISSUE": "ಸಮಸ್ಯೆಯನ್ನು ವರದಿ ಮಾಡಿ",
  "SUBMIT": "ಸಲ್ಲಿಸಿ",
  "VIEW_INVOICE": "ಸರಕುಪಟ್ಟಿ ವೀಕ್ಷಿಸಿ",
  "TOTAL_AMOUNT": "ಒಟ್ಟು ಮೊತ್ತ",
  "AMOUNT_PAID": "ಪಾವತಿಸಿದ ಮೊತ್ತ",
  "TRIP_DETAILS_": "ಪ್ರವಾಸದ ವಿವರಗಳು",
  "DOWNLOAD_PDF": "PDF ಅನ್ನು ಡೌನ್‌ಲೋಡ್ ಮಾಡಿ",
  "GST": "ಜಿಎಸ್ಟಿ",
  "INVOICE": "ಸರಕುಪಟ್ಟಿ",
  "TRIP_CHARGES": "ಟ್ರಿಪ್ ಶುಲ್ಕ",
  "PROMOTION": "ಪ್ರಚಾರ",
  "SEND_EMAIL": "ಇಮೇಲ್ ಕಳುಹಿಸಿ",
  "YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE": "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ಇಲ್ಲಿ ವಿವರಿಸಬಹುದು / ನೀವು ಎದುರಿಸಿದ ಸಮಸ್ಯೆಯನ್ನು ಇಲ್ಲಿ ವಿವರಿಸಬಹುದು",
  "THANK_YOU_FOR_WRITING": "ನಮಗೆ ಬರೆದಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE": "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ನಾವು ಸ್ವೀಕರಿಸಿದ್ದೇವೆ. ನಾವು ಸ್ವಲ್ಪ ಸಮಯದಲ್ಲಿ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತೇವೆ.",
  "GO_HOME_": "ಮನೆಗೆ ಹೋಗಿ",
  "ABOUT_APP_DESCRIPTION": "ನಮ್ಮಾ ಯಾತ್ರಿ ಸವಾರರನ್ನು ಚಾಲಕರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಮೀಟರ್ ದರದೊಂದಿಗೆ ಸವಾರಿಯನ್ನು ಕಾಯ್ದಿರಿಸಲು ಸವಾರರಿಗೆ ಅಪ್ಲಿಕೇಶನ್ ಅನುಕೂಲಕರವಾಗಿದೆ ಆದ್ದರಿಂದ ಕನಿಷ್ಠ ಶುಲ್ಕ",
  "ABOUT": "ಬಗ್ಗೆ",
  "PRIVACY_POLICY": "ಗೌಪ್ಯತಾ ನೀತಿ",
  "SET_UP_YOUR_ACCOUNT": "ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಹೊಂದಿಸಿ",
  "LOGO": "ಲೋಗಿ",
  "CONTINUE": "ಮುಂದುವರಿಸು",
  "ENTER_YOUR_NAME": "ನಿಮ್ಮ ಹೆಸರನ್ನು ನಮೂದಿಸಿ",
  "FULL_NAME": "ಪೂರ್ಣ ಹೆಸರು",
  "EMAIL": "ಇಮೇಲ್ ಮಾಡುವುದು",
  "WELCOME_TEXT": "ನಮ್ಮ ಯಾತ್ರೆಗೆ ಸುಸ್ವಾಗತ",
  "PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE": "ಮುಂದುವರಿಯಲು ದಯವಿಟ್ಟು ನಿಮ್ಮ ಆದ್ಯತೆಯ ಭಾಷೆಯನ್ನು ಆರಿಸಿ.",
  "WRITE_TO_US": "ನಮಗೆ ಬರೆಯಿರಿ",
  "NOTE": "ಸೂಚನೆ: ",
  "VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS": "ಸವಾರಿ ನಿರ್ದಿಷ್ಟ ದೂರುಗಳಿಗಾಗಿ ನನ್ನ ರೈಡ್ಸ್ ವಿಭಾಗಕ್ಕೆ ಭೇಟಿ ನೀಡಿ",
  "THANK_YOU_FOR_WRITING_TO_US": "ನಮಗೆ ಬರೆದಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME": "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ನಾವು ಸ್ವೀಕರಿಸಿದ್ದೇವೆ. ನಾವು ಕೆಲವೊಮ್ಮೆ ನಿಮ್ಮನ್ನು ತಲುಪುತ್ತೇವೆ.",
  "GO_TO_HOME__": "ಮನೆಗೆ ಹೋಗು",
  "SUBJECT": "ವಿಷಯ",
  "YOUR_EMAIL_ID": "ನಿಮ್ಮ ಇಮೇಲ್ ಐಡಿ",
  "DESCRIBE_YOUR_ISSUE": "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ವಿವರಿಸಿ",
  "ENTER_MOBILE_NUMBER": "ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ",
  "BY_TAPPING_CONTINUE": "ಮುಂದುವರಿಸಿ ಕ್ಲಿಕ್ ಮಾಡುವ ಮೂಲಕ",
  "TO_THE": "ನೀವು ಸ್ವೀಕರಿಸುತ್ತಿದ್ದೀರಿ ಎಂದು ನೀವು ಒಪ್ಪುತ್ತೀರಿ",
  "ENTER_OTP": "ಒಟಿಪಿ ನಮೂದಿಸಿ",
  "RESEND": "ಮರುಕಳುಹಿಸಿ",
  "ENTER_YOUR_MOBILE_NUMBER": "ನಿಮ್ಮ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ ",
  "LOGIN_USING_THE_OTP_SENT_TO": "ಕಳುಹಿಸಿದ ಒಟಿಪಿ ಬಳಸಿ ಲಾಗಿನ್ ಮಾಡಿ",
  "YOUR_RECENT_RIDE": "ನಿಮ್ಮ ಇತ್ತೀಚಿನ ಸವಾರಿ",
  "VIEW_ALL_RIDES": "ಎಲ್ಲಾ ಸವಾರಿಗಳನ್ನು ವೀಕ್ಷಿಸಿ",
  "ALL_TOPICS": "ಎಲ್ಲಾ ವಿಷಯಗಳು",
  "FAQ": "ಕಸಾಯಿಖಾನೆ",
  "REPORT_AN_ISSUE_WITH_THIS_TRIP": "ಈ ಪ್ರವಾಸದೊಂದಿಗೆ ಸಮಸ್ಯೆಯನ್ನು ವರದಿ ಮಾಡಿ",
  "YOU_RATED": "ನೀವು ರೇಟ್ ಮಾಡಿದ್ದೀರಿ:",
  "GETTING_STARTED_AND_FAQS": "ಪ್ರಾರಂಭಿಸುವುದು ಮತ್ತು FAQ ಗಳು",
  "FOR_OTHER_ISSUES_WRITE_TO_US": "ಇತರ ವಿಷಯಗಳಿಗಾಗಿ, ನಮಗೆ ಬರೆಯಿರಿ",
  "HELP_AND_SUPPORT": "ಸಹಾಯ ಮತ್ತು ಬೆಂಬಲ",
  "OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS": "ಈ ಪ್ರವಾಸಕ್ಕೆ ನಮ್ಮ ಸೂಚಿಸಿದ ಬೆಲೆ",
  "DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE": "*ಚಾಲಕರು ಮೇಲಿನ ಶ್ರೇಣಿಯ ನಡುವೆ ಚಾರ್ಜ್ ಮಾಡಬಹುದು",
  "HOW_THIS_WORKS": "ಇದು ಹೇಗೆ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತದೆ?",
  "FINDING_RIDES_NEAR_YOU": "ನಿಮ್ಮ ಹತ್ತಿರ ಸವಾರಿಗಳನ್ನು ಹುಡುಕುವುದು ...",
  "CONFIRMING_THE_RIDE_FOR_YOU": "ನಿಮಗಾಗಿ ಸವಾರಿಯನ್ನು ದೃ ming ೀಕರಿಸುತ್ತದೆ ...",
  "CANCEL_SEARCH": "ಹುಡುಕಾಟವನ್ನು ರದ್ದುಗೊಳಿಸಿ",
  "YOUR_RIDE_IS_NOW_COMPLETE": "ನಿಮ್ಮ ಸವಾರಿ ಈಗ ಪೂರ್ಣಗೊಂಡಿದೆ!",
  "PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH": "ದಯವಿಟ್ಟು ಅಂತಿಮ ಮೊತ್ತವನ್ನು ನೇರವಾಗಿ ಚಾಲಕನಿಗೆ ಪಾವತಿಸಿ",
  "WHERE_TO": "ಎಲ್ಲಿಗೆ?",
  "HOME": "ಮನೆ",
  "PICK_UP_LOCATION": "ಸ್ಥಳವನ್ನು ಆರಿಸಿ",
  "REQUEST_RIDE": "ವಿನಂತಿ ಸವಾರಿ",
  "NAME": "ಹೆಸರು",
  "MOBILE_NUMBER_STR": "ಮೊಬೈಲ್ ನಂಬರ",
  "PERSONAL_DETAILS": "ವೈಯಕ್ತಿಕ ವಿವರಗಳು",
  "YOUR_RIDES": "ನಿಮ್ಮ ಸವಾರಿಗಳು",
  "YOU_ARE_OFFLINE": "ನೀವು ಆಫ್‌ಲೈನ್‌ನಲ್ಲಿ",
  "CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN": "ನಿಮ್ಮ ಇಂಟರ್ನೆಟ್ ಸಂಪರ್ಕವನ್ನು ಪರಿಶೀಲಿಸಿ ಮತ್ತು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ",
  "TRY_AGAIN": "ಮತ್ತೆ ಪ್ರಯತ್ನಿಸು",
  "THANK_YOUR_DRIVER": "ನಿಮ್ಮ ಡ್ರೈವರ್‌ಗೆ ಧನ್ಯವಾದಗಳು!",
  "HOPE_YOUR_RIDE_WAS_HASSLE_FREE": "ನಿಮ್ಮ ಸವಾರಿ ಜಗಳ ಮುಕ್ತವಾಗಿದೆ ಎಂದು ಭಾವಿಸುತ್ತೇವೆ",
  "HOW_WAS_YOUR_RIDE_WITH": "ಜೊತೆಗಿನ ನಿಮ್ಮ ಸವಾರಿ ಹೇಗಿತ್ತು",
  "GOT_IT_TELL_US_MORE": "ಸಿಕ್ಕಿತು, ನಮಗೆ ಇನ್ನಷ್ಟು ಹೇಳಿ?",
  "WRITE_A_COMMENT": "ಕಾಮೆಂಟ್ ಬರೆಯಿರಿ (ಐಚ್ al ಿಕ)",
  "UPDATE": "ನವೀಕರಿಸು",
  "LANGUAGE": "ಭಾಷೆಗಳು",
  "OTP": "ಒತ್ತು",
  "PAYMENT_METHOD": "ಪಾವತಿ ವಿಧಾನ",
  "PAYMENT_METHOD_STRING": "ನಗದು / UPI ಅಪ್ಲಿಕೇಶನ್ ಬಳಸಿ",
  "CANCEL_RIDE": "ನನ್ನ ಸವಾರಿಯನ್ನು ರದ್ದುಮಾಡಿ",
  "SUPPORT": "ಬೆಂಬಲ",
  "PICKUP_AND_DROP": "ಪಿಕಪ್ ಮತ್ತು ಡ್ರಾಪ್",
  "CANCELLED": "ರದ್ದುಗೊಳಿಸಲಾಗಿದೆ",
  "HOW_THE_PRICING_WORKS": "ಬೆಲೆ ಹೇಗೆ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತದೆ?",
  "SELECT_AN_OFFER": "ಪ್ರಸ್ತಾಪವನ್ನು ಆಯ್ಕೆಮಾಡಿ",
  "CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT": "ನಿಮ್ಮ ಸೌಕರ್ಯದ ಪ್ರಕಾರ ಸವಾರಿಯನ್ನು ಆರಿಸಿ",
  "IT_SEEMS_TO_BE_A_VERY_BUSY_DAY": "ಇದು ತುಂಬಾ ಕಾರ್ಯನಿರತ ದಿನವೆಂದು ತೋರುತ್ತದೆ. ನೀವು ಮತ್ತೆ ಸವಾರಿಗಳನ್ನು ಹುಡುಕಲು ಪ್ರಯತ್ನಿಸಬಹುದು",
  "SORT_BY": "ವಿಂಗಡಿಸು",
  "SORRY_WE_COULDNT_FIND_ANY_RIDES": "ಕ್ಷಮಿಸಿ, ನಮಗೆ ಯಾವುದೇ ಸವಾರಿಗಳು ಸಿಗಲಿಲ್ಲ",
  "LOAD_MORE": "ಇನ್ನಷ್ಟು ಲೋಡ್ ಮಾಡಿ",
  "WE_NEED_ACCESS_TO_YOUR_LOCATION": "ನಿಮ್ಮ ಸ್ಥಳಕ್ಕೆ ನಮಗೆ ಪ್ರವೇಶ ಬೇಕು!",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ಆಟೋಗಳಿಂದ ಹತ್ತಿರವಿರುವ ಎಲ್ಲವನ್ನು ನಕ್ಷೆ ಮಾಡಲು ಮತ್ತು ಸಾಧ್ಯವಾದಷ್ಟು ತ್ವರಿತ ಸವಾರಿಯನ್ನು ಪಡೆಯಲು ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಮ್‌ಗೆ ಸಹಾಯ ಮಾಡುತ್ತದೆ.",
  "CALL": "ಕರೆಯಿಸು",
  "EMPTY_RIDES": "ಖಾಲಿ ಸವಾರಿಗಳು",
  "YOU_HAVENT_TAKEN_A_TRIP_YET": "ನೀವು ಇನ್ನೂ ಪ್ರವಾಸ ಕೈಗೊಂಡಿಲ್ಲ",
  "BOOK_NOW": "ಈಗ ಪುಸ್ತಕ",
  "DENY_ACCESS": "ಪ್ರವೇಶವನ್ನು ನಿರಾಕರಿಸು",
  "T_AND_C_A": "ಎ) ನೀವು ಬೀಟಾ ಪರೀಕ್ಷೆಯಲ್ಲಿ ಇಚ್ willing ೆಯ ಭಾಗವಹಿಸುವವರು ಮತ್ತು ಜಸ್ಪೇ ಯಾವುದೇ ವಿಷಯದಲ್ಲಿ ನಿಮ್ಮ ವಿರುದ್ಧ ಯಾವುದೇ ಹೊಣೆಗಾರಿಕೆಯನ್ನು ಹೊಂದಿರುವುದಿಲ್ಲ ಎಂದು ನೀವು ಒಪ್ಪುತ್ತೀರಿ",
  "TERMS_AND_CONDITIONS": " ಟಿ&ಸಿ ",
  "DATA_COLLECTION_AUTHORITY": "ಸಿ) ನಾನು ಈ ಮೂಲಕ ನನ್ನ ಮಾಹಿತಿಯನ್ನು ಸಂಗ್ರಹಿಸಲು Juspay ಗೆ ನೇಮಿಸುತ್ತೇನೆ ಮತ್ತು ಅಧಿಕಾರ ನೀಡುತ್ತೇನೆ ಮತ್ತು ಮುಂದುವರಿಸುವ ಮೂಲಕ, ನಾನು ಬಳಕೆಯ ನಿಯಮಗಳು ಮತ್ತು ಗೌಪ್ಯತೆ ನೀತಿಗೆ ಸಮ್ಮತಿಸುತ್ತೇನೆ",
  "PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL": "",
  "MANDATORY": "Mandatory",
  "SOFTWARE_LICENSE": "ಸಾಫ್ಟ್‌ವೇರ್ ಪರವಾನಗಿ",
  "LOGOUT_": "ಲಾಗ್ ಔಟ್",
  "REQUEST_AUTO_RIDE": "ನಿಮ್ಮ ಆಟೋ ರೈಡ್ ಅನ್ನು ಕಾಯ್ದಿರಿಸಿ",
  "RATE_YOUR_RIDE": "ನಿಮ್ಮ ಸವಾರಿಯನ್ನು ರೇಟ್ ಮಾಡಿ",
  "SKIP": "ಬಿಟ್ಟುಬಿಡಿ",
  "ERROR_404": "ದೋಷ 404",
  "PROBLEM_AT_OUR_END": "ನಮ್ಮ ತುದಿಯಲ್ಲಿ ಸಮಸ್ಯೆ ಇದೆ ಎಂದು ತೋರುತ್ತದೆ. ನಾವು ಮತ್ತೆ ಲಭ್ಯವಾದಾಗ ಸೂಚನೆ ಪಡೆಯಿರಿ",
  "NOTIFY_ME": "ನನಗೆ ತಿಳಿಸು",
  "ADDRESS": "ಭಾಷಣ",
  "CHANGE": "ಬದಲಾವಣೆ",
  "SAVE_AS": "ಉಳಿಸಿ",
  "ADD_TAG": "ಟ್ಯಾಗ್ ಸೇರಿಸಿ",
  "WORK": "ಕೆಲಸ",
  "OTHER": "ಬೇರೆ",
  "SAVE": "ಉಳಿಸಿ",
  "ADD_NEW_ADDRESS": "ಹೊಸ ವಿಳಾಸವನ್ನು ಸೇರಿಸಿ",
  "SAVED_ADDRESSES": "ಉಳಿಸಿದ ವಿಳಾಸಗಳು",
  "ADDRESSES": "ಉದ್ದೇಶ",
  "NO_FAVOURITES_SAVED_YET": "ಯಾವುದೇ ಮೆಚ್ಚಿನವುಗಳನ್ನು ಇನ್ನೂ ಉಳಿಸಲಾಗಿಲ್ಲ",
  "SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY": "ನಿಮ್ಮ ಆಗಾಗ್ಗೆ ಭೇಟಿ ನೀಡುವ ಸ್ಥಳಗಳನ್ನು ಸುಲಭವಾಗಿ ಇರಿಸಿಕೊಳ್ಳಲು ಮೆಚ್ಚಿನ ಸ್ಥಳವು ಸಹಾಯ ಮಾಡುತ್ತದೆ",
  "EMERGENCY_CONTACTS": "ತುರ್ತು ಸಂಪರ್ಕಗಳು",
  "ADD_EMERGENCY_CONTACTS": "ತುರ್ತು ಸಂಪರ್ಕಗಳನ್ನು ಸೇರಿಸಿ",
  "NO_EMERGENCY_CONTACTS_SET": "ತುರ್ತು ಸಂಪರ್ಕಗಳನ್ನು ಹೊಂದಿಸಲಾಗಿಲ್ಲ",
  "EMERGENCY_CONTACTS_SCREEN_DESCRIPTION": "ತುರ್ತು ಪರಿಸ್ಥಿತಿಯಲ್ಲಿ 3 ತುರ್ತು ಸಂಪರ್ಕಗಳ ಜೊತೆಗೆ ನಿಮ್ಮ ಸವಾರಿಯ \nಸ್ಥಿತಿಯನ್ನು ನೀವು ಹಂಚಿಕೊಳ್ಳಬಹುದು",
  "COPIED": "ನಕಲಿಸಿದ",
  "TRIP_ID": "ಪ್ರವಾಸದ ಐಡಿ",
  "SAVE_PLACE": "ಸ್ಥಳವನ್ನು ಉಳಿಸಿ",
  "RIDE_FARE": "ಸವಾರಿ ದರ",
  "ASK_FOR_PRICE": "ಬೆಲೆ ಕೇಳಿ",
  "ASK_FOR_PRICE_INFO": "ಚಾಲಕರು ಪ್ರಯಾಣಿಸುವ ಪಿಕ್-ಅಪ್ ದೂರಕ್ಕೆ ಹೆಚ್ಚುವರಿ ನಾಮಮಾತ್ರ ಶುಲ್ಕದೊಂದಿಗೆ ನೀವು ಸರ್ಕಾರ ನಿಗದಿಪಡಿಸಿದ ಮೂಲ ಬೆಲೆಗೆ ಪಡೆಯುತ್ತೀರಿ. ಕೆಲವು ಚಾಲಕರು ಟ್ರಾಫಿಕ್, ರಿಟರ್ನ್ ಟ್ರಿಪ್‌ನ ಸಾಧ್ಯತೆಗಳು ಇತ್ಯಾದಿ ಅಂಶಗಳನ್ನು ಒಳಗೊಳ್ಳಲು ತಮ್ಮ ವಿವೇಚನೆಯಿಂದ ನಾಮಮಾತ್ರದ ಸಲಹೆಗಳನ್ನು ಕೋರಬಹುದು.",
  "GET_ESTIMATE_FARE": "ಅಂದಾಜು ದರವನ್ನು ಪಡೆಯಿರಿ",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS": "ಪ್ರಸ್ತಾಪವನ್ನು ಆಯ್ಕೆಮಾಡಿ (ಐಚ್ಛಿಕ)",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO": "ಪೂರ್ವನಿಯೋಜಿತವಾಗಿ, “ಸವಾರಿಯನ್ನು ಸ್ವಯಂ ನಿಯೋಜಿಸಿ” ಅನ್ನು ಸಕ್ರಿಯಗೊಳಿಸಿದಾಗ ಅಂದಾಜು ವ್ಯಾಪ್ತಿಯೊಳಗೆ ಮೊದಲು ಸ್ವೀಕರಿಸುವ ಚಾಲಕನೊಂದಿಗೆ ನಿಮ್ಮನ್ನು ನಿಯೋಜಿಸಲಾಗುತ್ತದೆ. ಬದಲಿಗೆ, ನೀವು ಚಾಲಕ ಕೊಡುಗೆಯನ್ನು ಆಯ್ಕೆ ಮಾಡಲು ಬಯಸಿದರೆ, ಈ ವೈಶಿಷ್ಟ್ಯವನ್ನು ಬಳಸಲು ನೀವು ಆಯ್ಕೆಯನ್ನು ರದ್ದುಗೊಳಿಸಬಹುದು",
  "PAY_THE_DRIVER": "ಚಾಲಕನಿಗೆ ಪಾವತಿಸಿ",
  "PAY_THE_DRIVER_INFO": "ನೀವು ದೃಢೀಕರಿಸಿದ ಬೆಲೆಯನ್ನು ಚಾಲಕನಿಗೆ ಪಾವತಿಸಿ. ",
  "PAY_THE_DRIVER_NOTE": "(ಪ್ರಯಾಣದ ಅಂತರ ಬದಲಾದರೆ ಒಟ್ಟು ದರ ಬದಲಾಗಬಹುದು)",
  "UPDATE_PERSONAL_DETAILS": "ವೈಯಕ್ತಿಕ ವಿವರಗಳನ್ನು ನವೀಕರಿಸಿ",
  "EDIT": "ತಿದ್ದು",
  "DEL_ACCOUNT": "ಖಾತೆ ಅಳಿಸಿ",
  "ACCOUNT_DELETION_CONFIRMATION": "ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಅಳಿಸಲು ನೀವು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ? ನಿಮ್ಮ ಎಲ್ಲ ವೈಯಕ್ತಿಕ ಡೇಟಾ ಕಳೆದು ಹೋಗುತ್ತದೆ",
  "REQUEST_SUBMITTED": "ಕೋರಿಕೆ ಸಲ್ಲಿಸಲಾಗಿದೆ",
  "WE_WILL_DELETE_YOUR_ACCOUNT": "ನೀವು ನಮ್ಮ ಪ್ಲಾಟ್‌ಫಾರ್ಮ್ ಅನ್ನು ತೊರೆಯುವುದನ್ನು ನೋಡಿ ನಮಗೆ ವಿಷಾದವಿದೆ ಮುಂದಿನ 30 ದಿನಗಳಲ್ಲಿ ನಾವು ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಅಳಿಸುತ್ತೇವೆ. ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಉಳಿಸಿಕೊಳ್ಳಲು ನೀವು ಬಯಸಿದರೆ, ದಯವಿಟ್ಟು ನಮ್ಮ ಬೆಂಬಲ ಸಂಖ್ಯೆಗೆ ಕರೆ ಮಾಡಿ",
  "YES_DELETE_IT": "ಹೌದು, ಅಳಿಸಿ",
  "REQUEST_TO_DELETE_ACCOUNT": "ಖಾತೆಯನ್ನು ಅಳಿಸಲು ವಿನಂತಿ",
  "CANCEL_STR": "ರದ್ದುಮಾಡು",
  "LOADING": "ಲೋಡ್ ಆಗುತ್ತಿದೆ",
  "PLEASE_WAIT_WHILE_IN_PROGRESS": "ಪ್ರಗತಿಯಲ್ಲಿರುವಾಗ ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ",
  "SET_LOCATION_ON_MAP": "ನಕ್ಷೆಯಲ್ಲಿ ಸ್ಥಳವನ್ನು ಹೊಂದಿಸಿ",
  "CURRENT_LOCATION": "ಈಗಿನ ಸ್ಥಳ",
  "I_AM_NOT_RECEIVING_ANY_RIDES": "ನಾನು ಯಾವುದೇ ಸವಾರಿಗಳನ್ನು ಸ್ವೀಕರಿಸುತ್ತಿಲ್ಲ",
  "DELETE": "ಅಳಿಸಿ",
  "ARE_YOU_SURE_YOU_WANT_TO_LOGOUT": "ನೀವು ಲಾಗ್ಔಟ್ ಮಾಡಲು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "ನೀವು ರದ್ದುಗೊಳಿಸಲು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "ನೀವು ಸವಾರಿ ಕೊಡುಗೆಗಳನ್ನು ಹೊಂದಿದ್ದೀರಿ, ನೀವು ರದ್ದುಗೊಳಿಸಲು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "GO_BACK_": "ಹಿಂದೆ ಹೋಗು",
  "REGISTER_USING_DIFFERENT_NUMBER": "ನೀವು ಬೇರೆ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ಬಳಸಿಕೊಂಡು ನೋಂದಾಯಿಸಲು ಬಯಸುವಿರಾ?",
  "YES": "ಹೌದು",
  "NO": "ಸಂ",
  "CANCEL_": "ರದ್ದುಮಾಡಿ",
  "IS_ON_THE_WAY": "ದಾರಿಯಲ್ಲಿದೆ..",
  "ENTER_4_DIGIT_OTP": "4 ಅಂಕಿಯ OTP ನಮೂದಿಸಿ",
  "WRONG_OTP": "ತಪ್ಪಾದ OTP",
  "GRANT_ACCESS": "ಪ್ರವೇಶವನ್ನು ನೀಡಿ",
  "ENTER_A_LOCATION": "ಒಂದು ಸ್ಥಳವನ್ನು ನಮೂದಿಸಿ",
  "NEARBY": "ಹತ್ತಿರದಲ್ಲಿದೆ",
  "MINS_AWAY": "ನಿಮಿಷಗಳ ದೂರ",
  "PAID": "ಪಾವತಿಸಲಾಗಿದೆ",
  "BY_CASH": "ನಗದು ಮೂಲಕ",
  "ONLINE_": "ಆನ್ಲೈನ್",
  "USER": "ಬಳಕೆದಾರ",
  "EMAIL_ALREADY_EXISTS": "ಇಮೇಲ್ ಅನ್ನು ನವೀಕರಿಸಲು ವಿಫಲವಾಗಿದೆ. ಇಮೇಲ್ ಈಗಾಗಲೇ ಅಸ್ತಿತ್ವದಲ್ಲಿದೆ.",
  "IN": "ಒಳಗೆ",
  "VERIFYING_OTP": "ಒಟಿಪಿ ಪರಿಶೀಲಿಸಲಾಗುತ್ತಿದೆ",
  "TRACK_LIVE_LOCATION_USING": "ಬಳಸಿಕೊಂಡು ಲೈವ್ ಸ್ಥಳವನ್ನು ಟ್ರ್ಯಾಕ್ ಮಾಡಿ",
  "GOOGLE_MAP_": "ಗೂಗಲ್ ನಕ್ಷೆ",
  "IN_APP_TRACKING": "ಅಪ್ಲಿಕೇಶನ್ ಟ್ರ್ಯಾಕಿಂಗ್‌ನಲ್ಲಿ",
  "REQUEST_TIMED_OUT": "ವಿನಂತಿಯ ಸಮಯ ಮೀರಿದೆ",
  "LIMIT_EXCEEDED": "ಮಿತಿ ಮೀರಿದೆ",
  "ERROR_OCCURED": "ದೋಷ ಸಂಭವಿಸಿದೆ",
  "QUOTE_EXPIRED": "ಉಲ್ಲೇಖದ ಅವಧಿ ಮುಗಿದಿದೆ",
  "GETTING_ESTIMATES_FOR_YOU": "ನಿಮಗಾಗಿ ಅಂದಾಜುಗಳನ್ನು ಪಡೆಯಲಾಗುತ್ತಿದೆ...",
  "CONFIRM_PICKUP_LOCATION": "ಪಿಕಪ್ ಸ್ಥಳವನ್ನು ದೃಢೀಕರಿಸಿ",
  "CONFIRM_DROP_LOCATION": "ಡ್ರಾಪ್ ಸ್ಥಳವನ್ನು ದೃಢೀಕರಿಸಿ",
  "NO_DRIVERS_AVAILABLE": "ಯಾವುದೇ ಚಾಲಕರು ಲಭ್ಯವಿಲ್ಲ",
  "ERROR_OCCURED_TRY_AGAIN": "ದೋಷ ಸಂಭವಿಸಿದೆ. ಮತ್ತೆ ಪ್ರಯತ್ನಿಸು",
  "ERROR_OCCURED_TRY_AFTER_SOMETIME": "ದೋಷ ಸಂಭವಿಸಿದೆ. ದಯವಿಟ್ಟು ಪುನಃ ಪ್ರಯತ್ನಿಸಿ",
  "ASKED_FOR_MORE_MONEY": "ಹೆಚ್ಚಿನ ಹಣ ಕೇಳಿದರು",
  "START_": "ಪ್ರಾರಂಭಿಸಿ",
  "LIMIT_REACHED": "ಮಿತಿಯನ್ನು ತಲುಪಿದೆ. ಸ್ವಲ್ಪ ಸಮಯದ ನಂತರ ಪ್ರಯತ್ನಿಸಿ",
  "RIDE_NOT_SERVICEABLE": "ರೈಡ್ ಸರ್ವಿಸಬಲ್ ಅಲ್ಲ",
  "CONFIRM_FOR": "ಗಾಗಿ ದೃಢೀಕರಿಸಿ",
  "ETA_WAS_TOO_SHORT": "ETA ತುಂಬಾ ಚಿಕ್ಕದಾಗಿದೆ.",
  "DRIVER_REQUESTED_TO_CANCEL": "ರದ್ದುಗೊಳಿಸಲು ಚಾಲಕ ನನ್ನನ್ನು ವಿನಂತಿಸಿದನು",
  "PICK_UP_LOCATION_INCORRECT": "ಪಿಕಪ್ ಸ್ಥಳವು ತಪ್ಪಾಗಿದೆ.",
  "COULD_NOT_CONNECT_TO_DRIVER": "ನನಗೆ ಡ್ರೈವರ್‌ಗೆ ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ.",
  "ETA_WAS_TOO_LONG": "ETA ತುಂಬಾ ಉದ್ದವಾಗಿದೆ.",
  "OTHERS": "ಇತರರು",
  "DESTINATION_OUTSIDE_LIMITS": "ನಮೂದಿಸಿದ ಗಮ್ಯಸ್ಥಾನವು ನಗರ ಮಿತಿಯಿಂದ ಹೊರಗಿದೆ",
  "DROP_LOCATION_FAR_AWAY": "ನಿಮ್ಮ ಡ್ರಾಪ್ ಸ್ಥಳವು ತುಂಬಾ ದೂರದಲ್ಲಿದೆ",
  "CHANGE_DROP_LOCATION": "ಡ್ರಾಪ್ ಸ್ಥಳವನ್ನು ಬದಲಾಯಿಸಿ",
  "YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING": "ನೀವು ನಡೆಯಬಹುದು ಅಥವಾ ರೈಡ್ ಬುಕ್ ಮಾಡುವುದನ್ನು ಮುಂದುವರಿಸಬಹುದು",
  "YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST": "ನಿಮ್ಮ ಪ್ರವಾಸವು ತುಂಬಾ ಚಿಕ್ಕದಾಗಿದೆ. ನಿಮ್ಮ ಗಮ್ಯಸ್ಥಾನದಿಂದ ನೀವು ಕೇವಲ ",
  "METERS_AWAY_FROM_YOUR_DESTINATION": " ಮೀ ದೂರದಲ್ಲಿದ್ದೀರಿ!",
  "BOOK_RIDE_": "ರೈಡ್ ಬುಕ್ ಮಾಡಿ",
  "LOCATION_UNSERVICEABLE": "ಸ್ಥಳವು ಸೇವೆಗೆ ಅರ್ಹವಲ್ಲ",
  "CURRENTLY_WE_ARE_LIVE_IN_": "ಪ್ರಸ್ತುತ ನಾವು ಬೆಂಗಳೂರು ಮತ್ತು ಮೈಸೂರಿನಲ್ಲಿ ಸೇವೆ ಮಾಡುತ್ತಿದ್ದೇವೆ, ನೀವು ಅಲ್ಲಿ ನಮ್ಮ ಸೇವೆಗಳನ್ನು ಆನಂದಿಸಬಹುದು",
  "CHANGE_LOCATION": "ಸ್ಥಳವನ್ನು ಬದಲಾಯಿಸಿ",
  "IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE": "ನೀವು ಇನ್ನೂ ರೈಡ್ ಅನ್ನು ಬುಕ್ ಮಾಡಲು ಬಯಸಿದರೆ ಮುಂದುವರಿಸಿ ಕ್ಲಿಕ್ ಮಾಡಿ ಮತ್ತು ರೈಡ್ ಅನ್ನು ಬುಕ್ ಮಾಡಲು ಪ್ರಾರಂಭಿಸಿ",
  "THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE": "ಪ್ರವಾಸವು ತುಂಬಾ ಚಿಕ್ಕದಾಗಿದೆ ಮತ್ತು ಪೂರ್ಣಗೊಳಿಸಲು ಕೇವಲ ",
  "STEPS_TO_COMPLETE": " ಹಂತಗಳು ಬೇಕಾಗುತ್ತವೆ",
  "CANCEL_AUTO_ASSIGNING": "ಸ್ವಯಂ ನಿಯೋಜಿಸುವುದನ್ನು ರದ್ದುಗೊಳಿಸಿ",
  "AUTO_ACCEPTING_SELECTED_RIDE": "ಸೆಕೆಂಡ್‌ನಲ್ಲಿ ಸ್ವಯಂ ಸ್ವೀಕರಿಸಲಾಗುತ್ತಿದೆ",
  "HELP_US_WITH_YOUR_REASON": "ನಿಮ್ಮ ಕಾರಣದೊಂದಿಗೆ ನಮಗೆ ಸಹಾಯ ಮಾಡಿ",
  "MAX_CHAR_LIMIT_REACHED": "ಗರಿಷ್ಠ ಅಕ್ಷರ ಮಿತಿಯನ್ನು ತಲುಪಿದೆ,",
  "SHOW_ALL_OPTIONS": "ಎಲ್ಲಾ ಆಯ್ಕೆಗಳನ್ನು ತೋರಿಸಿ",
  "DRIVER_WAS_NOT_REACHABLE": "ಚಾಲಕನನ್ನು ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ",
  "EXPIRES_IN": "ಸೆಕೆಂಡುಗಳಲ್ಲಿ ಅವಧಿ ಮುಗಿಯುತ್ತದೆ",
  "PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI": "*ನಗದು/ಯುಪಿಐ ಬಳಸಿ ನಿಮ್ಮ ಡ್ರೈವರ್‌ಗೆ ನೇರವಾಗಿ ಪಾವತಿಸಿ",
  "UPDATE_REQUIRED": "ಅಪ್ಡೇಟ್ ಅಗತ್ಯವಿದೆ",
  "PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE": "ಸೇವೆಯನ್ನು ಮುಂದುವರಿಸಲು ದಯವಿಟ್ಟು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ನವೀಕರಿಸಿ",
  "NOT_NOW": "ಈಗಲ್ಲ",
  "OF": "ರಲ್ಲಿ",
  "LOST_SOMETHING": "ಏನನ್ನಾದರೂ ಕಳೆದುಕೊಂಡಿದ್ದೀರಾ?",
  "TRY_CONNECTING_WITH_THE_DRIVER": "ಡ್ರೈವರ್‌ನೊಂದಿಗೆ ನೇರವಾಗಿ ಸಂಪರ್ಕಿಸಲು ನೀವು ಕಾಲ್‌ಬ್ಯಾಕ್ ಅನ್ನು ವಿನಂತಿಸಬಹುದು",
  "CALL_DRIVER": "ಚಾಲಕನೊಂದಿಗೆ ಸಂಪರ್ಕಪಡಿಸಿ",
  "NO_MORE_RIDES": "ಇನ್ನು ರೈಡ್‌ಗಳಿಲ್ಲ",
  "CONTACT_SUPPORT": "ಬೆಂಬಲವನ್ನು ಸಂಪರ್ಕಿಸಿ",
  "INVALID_MOBILE_NUMBER": "ಅಮಾನ್ಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆ",
  "RIDE_COMPLETED": "ಸವಾರಿ ಪೂರ್ಣಗೊಂಡಿದೆ",
  "CONFIRM_LOCATION": "ಸ್ಥಳವನ್ನು ದೃಢೀಕರಿಸಿ",
  "SUBMIT_FEEDBACK": "ಪ್ರತಿಕ್ರಿಯೆಯನ್ನು ಸಲ್ಲಿಸಿ",
  "HOW_WAS_YOUR_RIDE_EXPERIENCE": "ನಿಮ್ಮ ಸವಾರಿಯ ಅನುಭವ ಹೇಗಿತ್ತು?",
  "DROP": "ಡ್ರಾಪ್",
  "RATE_YOUR_RIDE_WITH": " ಜೊತೆಗೆ ನಿಮ್ಮ ಸವಾರಿಯನ್ನು ರೇಟ್ ಮಾಡಿ",
  "VIEW_BREAKDOWN": "ವಿಭಜನೆಯನ್ನು ವೀಕ್ಷಿಸಿ",
  "PAY_DRIVER_USING_CASH_OR_UPI": "Cash/UPI ಬಳಸಿಕೊಂಡು ಚಾಲಕನಿಗೆ ಪಾವತಿಸಿ",
  "RATE_YOUR_DRIVER": "ನಿಮ್ಮ ಡ್ರೈವರ್ ಅನ್ನು ರೇಟ್ ಮಾಡಿ",
  "MY_RIDES": "ನನ್ನ ಸವಾರಿಗಳು",
  "RIDE_ID": "ರೈಡ್ ಐಡಿ",
  "RIDE_DETAILS": "ಸವಾರಿಯ ವಿವರಗಳು",
  "SELECT_A_RIDE": "ಹೆಚ್ಚಿನ ವಿವರಗಳಿಗಾಗಿ ಸವಾರಿಯನ್ನು ಆಯ್ಕೆಮಾಡಿ",
  "CONFIRM_RIDE_": "ರೈಡ್ ಅನ್ನು ದೃಢೀಕರಿಸಿ",
  "YOU_CAN_CANCEL_RIDE": "ನೀವು ಚಾಲಕರಿಂದ ಪ್ರಸ್ತಾಪವನ್ನು ಪಡೆದ ನಂತರ ನೀವು ರದ್ದುಗೊಳಿಸಬಹುದು",
  "ESTIMATES_CHANGED": "ನಿಮ್ಮ ಸವಾರಿಯ ಅಂದಾಜು ಬದಲಾಗಿದೆ ",
  "ESTIMATES_REVISED_TO": "ಪರಿಷ್ಕೃತ ಅಂದಾಜು",
  "RATE_CARD": "ದರದ ಚೀಟಿ",
  "NIGHT_TIME_CHARGES": "ರಾತ್ರಿ ಸಮಯದ ಶುಲ್ಕಗಳು",
  "MIN_FARE_UPTO": "2 ಕಿಮೀ ವರೆಗೆ ಕನಿಷ್ಠ ದರ",
  "RATE_ABOVE_MIN_FARE": "ಕನಿಷ್ಠ ದರಕ್ಕಿಂತ ಹೆಚ್ಚಿನ ದರ",
  "DRIVER_PICKUP_CHARGES": "ಚಾಲಕ ಪಿಕಪ್ ಶುಲ್ಕಗಳು",
  "DAY_TIMES_OF": "",
  "NIGHT_TIMES_OF": "ರಾತ್ರಿ (🌙) 10 ರಿಂದ 5 AM ವರೆಗೆ ಹಗಲಿನ ಶುಲ್ಕದ ",
  "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT": " ಬಾರಿ ಹಗಲಿನ ಶುಲ್ಕಗಳು ರಾತ್ರಿಯಲ್ಲಿ ಅನ್ವಯಿಸುತ್ತದೆ 10 PM ರಿಂದ 5 AM",
  "DAYTIME_CHARGES_APPLIED_AT_NIGHT": "x ಅನ್ನು ರಾತ್ರಿಯ ದರಕ್ಕೆ ಅನ್ವಯಿಸಲಾಗುತ್ತದೆ",
  "DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "* ಟ್ರಾಫಿಕ್, ರಿಟರ್ನ್ ಟ್ರಿಪ್‌ನ ಅವಕಾಶ ಇತ್ಯಾದಿಗಳನ್ನು ಸರಿದೂಗಿಸಲು ಚಾಲಕ ಐಚ್ಛಿಕವಾಗಿ 10% ಮೂಲ ದರವನ್ನು (ಹತ್ತಿರದ ರೂ. 10 ಗೆ ದುಂಡಾಗಿರುತ್ತದೆ) ವಿನಂತಿಸಬಹುದು.",
  "GOT_IT": "ಗೊತ್ತಾಯಿತು!",
  "DAY_TIME_CHARGES": "ಹಗಲಿನ ಶುಲ್ಕಗಳು",
  "SHARE_APP": "ಅಪ್ಲಿಕೇಶನ್ ಹಂಚಿಕೊಳ್ಳಿ",
  "AWAY": "ದೂರ",
  "AWAY_C": "ದೂರ",
  "AT_PICKUP": "At Pickup",
  "FARE_UPDATED": "ದರವನ್ನು ನವೀಕರಿಸಲಾಗಿದೆ",
  "TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE": "ಮಾರ್ಗದಲ್ಲಿನ ಬದಲಾವಣೆಯಿಂದಾಗಿ ಒಟ್ಟು ದರ ಬದಲಾಗಬಹುದು",
  "AT_DROP": "At Drop",
  "EMERGENCY_HELP": "ತುರ್ತು ಸಹಾಯ",
  "CALL_POLICE": "ಪೊಲೀಸರಿಗೆ ಕರೆ ಮಾಡಿ",
  "ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION": "ನಿಮ್ಮ ಸವಾರಿಯ ಸ್ಥಿತಿ ಮತ್ತು ಸ್ಥಳವನ್ನು ಸಹ ಹಂಚಿಕೊಳ್ಳಿ",
  "SHARE_RIDE_WITH_EMERGENCY_CONTACTS": "ತುರ್ತು ಸಂಪರ್ಕಗಳೊಂದಿಗೆ ರೈಡ್ ಅನ್ನು ಹಂಚಿಕೊಳ್ಳಿ",
  "DO_YOU_NEED_EMERGENCY_HELP": "ನಿಮಗೆ ತುರ್ತು ಸಹಾಯ ಬೇಕೇ ?",
  "CALL_SUPPORT": "ಕರೆ ಬೆಂಬಲ",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "ನೀವು ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?",
  "YOU_ARE_ABOUT_TO_CALL_POLICE": "ನೀವು ಪೊಲೀಸರಿಗೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?",
  "DAIL_100": "ಡಯಲ್ 100?",
  "HELP_US_WITH_YOUR_FEEDBACK": "ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆಯೊಂದಿಗೆ ನಮಗೆ ಸಹಾಯ ಮಾಡಿ",
  "WAIT_TIME": "ಸಮಯ ಕಾಯಿರಿ",
  "FAVOURITES": "ಮೆಚ್ಚಿನವುಗಳು",
  "ADD_FAVOURITE": "ಮೆಚ್ಚಿನವುಗಳನ್ನು ಸೇರಿಸಿ",
  "ALL_FAVOURITES": "ನೆಚ್ಚಿನ ಸ್ಥಳ",
  "REMOVE": "ತೆಗೆದುಹಾಕಿ",
  "SELECT_ON_MAP": "ನಕ್ಷೆಯಲ್ಲಿ ಆಯ್ಕೆಮಾಡಿ",
  "FAVOURITE_LOCATION": "ಮೆಚ್ಚಿನ ಸ್ಥಳ",
  "EDIT_FAVOURITE": "ಮೆಚ್ಚಿನ ಸಂಪಾದಿಸು",
  "DRAG_THE_MAP": "ನಕ್ಷೆಯನ್ನು ಎಳೆಯಿರಿ ಮತ್ತು ನಿಖರವಾದ ಸ್ಥಳಕ್ಕೆ ಪಿನ್ ಹೊಂದಿಸಿ",
  "CHOOSE_ON_MAP": "ನಕ್ಷೆಯಲ್ಲಿ ಆಯ್ಕೆಮಾಡಿ",
  "USE_CURRENT_LOCATION": "ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಬಳಸಿ",
  "FAVOURITE_YOUR_CURRENT_LOCATION": "ನಿಮ್ಮ ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಮೆಚ್ಚಿಕೊಳ್ಳಿ",
  "LOCATION": "ಸ್ಥಳ",
  "LOCATION_ALREADY_EXISTS_AS": "ಸಈಗಾಗಲೇ ಅಸ್ತಿತ್ವದಲ್ಲಿದೆ",
  "GIVE_THIS_LOCATION_A_NAME": "ಸ್ಥಳಕ್ಕೆ ಹೆಸರನ್ನು ನೀಡಿ",
  "FAVOURITE": "ನೆಚ್ಚಿನ",
  "CONFIRM_AND_SAVE": "ದೃಢೀಕರಿಸಿ ಮತ್ತು ಉಳಿಸಿ",
  "REMOVE_FAVOURITE": "ಮೆಚ್ಚಿನವನ್ನು ತೆಗೆದುಹಾಕಿ",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_": "ನೆಚ್ಚಿನ ಸ್ಥಳವನ್ನು ತೆಗೆದುಹಾಕಲು ನೀವು \n ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "YES_REMOVE": "ಹೌದು, ತೆಗೆದುಹಾಕಿ",
  "ADD_NEW_FAVOURITE": "ಹೊಸ ಮೆಚ್ಚಿನವನ್ನು ಸೇರಿಸಿ",
  "SELECT_YOUR_DROP": "ನಿಮ್ಮ ಡ್ರಾಪ್ ಆಯ್ಕೆಮಾಡಿ",
  "FAVOURITE_REMOVED_SUCCESSFULLY": "ಮೆಚ್ಚಿನವುಗಳನ್ನು ಯಶಸ್ವಿಯಾಗಿ ತೆಗೆದುಹಾಕಲಾಗಿದೆ",
  "LOCATION_ALREADY_EXISTS": "ಲೋಕೇಶನ್ ಈಗಾಗಲೇ ಇದೆ",
  "FAVOURITE_LIMIT_REACHED": "ಮೆಚ್ಚಿನ ಮಿತಿಯನ್ನು ತಲುಪಿದೆ",
  "LOCATION_ALREADY": "ಸ್ಥಳವು ಈಗಾಗಲೇ",
  "EXISTS_AS": "ಆಗಿ ಅಸ್ತಿತ್ವದಲ್ಲಿದೆ",
  "FAVOURITE_ADDED_SUCCESSFULLY": "ಮೆಚ್ಚಿನವುಗಳನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಸೇರಿಸಲಾಗಿದೆ",
  "FAVOURITE_UPDATED_SUCCESSFULLY": "ಮೆಚ್ಚಿನವು ಯಶಸ್ವಿಯಾಗಿ ನವೀಕರಿಸಲಾಗಿದೆ",
  "ALREADY_EXISTS": "ಈಗಾಗಲೇ ಇದೆ",
  "NAME_ALREADY_IN_USE": "ಹೆಸರು ಈಗಾಗಲೇ ಬಳಕೆಯಲ್ಲಿದೆ",
  "SELECT_FAVOURITE": "ಮೆಚ್ಚಿನದನ್ನು ಆಯ್ಕೆಮಾಡಿ",
  "CONFIRM_CHANGES": "ಬದಲಾವಣೆಗಳನ್ನು ದೃಢೀಕರಿಸಿ",
  "ADD_SAVED_LOCATION_FROM_SETTINGS": "*ನೀವು ಸೈಡ್ ಮೆನು > ಮೆಚ್ಚಿನವುಗಳಿಂದ ಹೊಸ ಮೆಚ್ಚಿನವುಗಳನ್ನು ಸೇರಿಸಬಹುದು",
  "YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS": "3 ಸಂಪರ್ಕಗಳವರೆಗೆ ಆಯ್ಕೆ ಮಾಡಲು ನಿಮ್ಮನ್ನು ಕೇಳಲಾಗುತ್ತದೆ",
  "AUTO_ASSIGN_A_RIDE": "ಸವಾರಿಯನ್ನು ಸ್ವಯಂ ನಿಯೋಜಿಸಿ",
  "IS_WAITING_FOR_YOU": "ಕಾಯುತ್ತಿದ್ದಾರೆ...",
  "WAIT_TIME_TOO_LONG": "ತುಂಬಾ ಸಮಯ ಕಾಯಿರಿ",
  "GOT_ANOTHER_RIDE_ELSE_WHERE": "ಮತ್ತೆಲ್ಲೋ ಮತ್ತೊಂದು ಸವಾರಿ ಸಿಕ್ಕಿತು",
  "DRIVER_WAS_RUDE": "ಚಾಲಕ ಅಸಭ್ಯವಾಗಿ ವರ್ತಿಸಿದ",
  "MAYBE_LATER": "ಬಹುಶಃ ನಂತರ",
  "YOUR_RIDE_HAS_STARTED": "ವಾಹ್! ನಿಮ್ಮ ಸವಾರಿ ಪ್ರಾರಂಭವಾಗಿದೆ 🤩",
  "ENJOY_RIDING_WITH_US": "ನಮ್ಮೊಂದಿಗೆ ಸವಾರಿ ಮಾಡುವುದನ್ನು ಆನಂದಿಸುತ್ತೀರಾ? ಸುದ್ದಿಯನ್ನು  \n ಹರಡಿ ಮತ್ತು ಸಂತೋಷವನ್ನು ಹಂಚಿಕೊಳ್ಳಿ",
  "VIEW_DETAILS": "ವಿವರಗಳನ್ನು ವೀಕ್ಷಿಸಿ",
  "REPEAT_RIDE": "ಸವಾರಿಯನ್ನು ಪುನರಾವರ್ತಿಸಿ",
  "FARE_WAS_HIGH": "ಪ್ರಯಾಣ ದರ ಹೆಚ್ಚಾಗಿತ್ತು",
  "AUTO_ASSIGN_DRIVER": "ಚಾಲಕವನ್ನು ಸ್ವಯಂ ನಿಯೋಜಿಸಿ",
  "CHOOSE_BETWEEN_MULTIPLE_DRIVERS": "ಬಹು ಚಾಲಕರ ನಡುವೆ ಆಯ್ಕೆಮಾಡಿ",
  "CHOOSE_BETWEEN_MULTIPLE_RIDES": "ಮಲ್ಟಿಪಲ್ ಸವಾರಿಗಳಿಂದ ಆಯ್ಕೆಮಾಡಿ",
  "ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE": "ನಿಮ್ಮ ಸವಾರಿಯನ್ನು ಆಯ್ಕೆಮಾಡಲು ಈ ವೈಶಿಷ್ಟ್ಯವನ್ನು ಸಕ್ರಿಯಗೊಳಿಸಿ",
  "BOOKING_PREFERENCE": "ಬುಕಿಂಗ್ ಆದ್ಯತೆ",
  "BASE_FARES": "ಮೂಲ ದರ",
  "PICKUP_CHARGE": "ಚಾಲಕ ಪಿಕಪ್ ಶುಲ್ಕಗಳು",
  "TOTAL_PAID": "ಒಟ್ಟು ಪಾವತಿಸಲಾಗಿದೆ",
  "WAITING_CHARGE": "ಕಾಯುವ ಶುಲ್ಕಗಳು**",
  "NOMINAL_FARE": "ಐಚ್ಛಿಕ ಚಾಲಕ ವಿನಂತಿ*",
  "DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO": "* ಟ್ರಾಫಿಕ್, ರಿಟರ್ನ್ ಟ್ರಿಪ್‌ನ ಸಾಧ್ಯತೆಗಳು ಮುಂತಾದ ಇತರ ಅಂಶಗಳನ್ನು ಸರಿದೂಗಿಸಲು ಚಾಲಕರು ಐಚ್ಛಿಕವಾಗಿ 10% ಮೂಲ ದರವನ್ನು (ಹತ್ತಿರದ ರೂ. 10 ಕ್ಕೆ ದುಂಡಾದ) ಕೋರಬಹುದು.",
  "WAITING_CHARGE_DESCRIPTION": "** ಕಾಯುವ ಶುಲ್ಕಗಳು: ₹1 / ನಿಮಿಷ - ಚಾಲಕರು ಆಗಮಿಸಿದ 3 ನಿಮಿಷಗಳ ನಂತರ",
  "SUCCESSFUL_ONBOARD": "ನೀವು ಯಶಸ್ವಿಯಾಗಿ ನಮ್ಮ ಯಾತ್ರಿಗೆ ಸೈನ್ ಇನ್ ಆಗಿರುವಿರಿ",
  "HAVE_REFERRAL_CODE": "ರೆಫರಲ್ ಕೋಡ್ ಇದೆಯೇ?",
  "REFEREAL_CODE_DISCRIPTION": "ನಿಮ್ಮ ರೆಫರಲ್ ಚಾಲಕನಿಗೆ ಬಹುಮಾನ \nನೀಡಲು ಕಾರಣವಾಗಿರಬಹುದು!",
  "SIX_DIGIT_REFERRAL_CODE": "ಚಾಲಕ ಹಂಚಿಕೊಂಡಿರುವ 6 ಅಂಕಿಗಳ ಕೋಡ್ ನಮೂದಿಸಿ",
  "ABOUT_REFERRAL_PROGRAM": "ರೆಫರಲ್ ಪ್ರೋಗ್ರಾಂ ಎಂದರೇನು?",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "ರೆಫರಲ್ ಪ್ರೋಗ್ರಾಂ ಡ್ರೈವರ್‌ಗಳನ್ನು ಹೆಚ್ಚು ಸವಾರಿಗಳನ್ನು ಸ್ವೀಕರಿಸಲು, ಕಡಿಮೆ ರದ್ದುಗೊಳಿಸಲು ಮತ್ತು ಯೋಗ್ಯ ಚಾಲಕರನ್ನು ಗುರುತಿಸುವ ಮತ್ತು ಬಹುಮಾನ ನೀಡುವ ಮೂಲಕ ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸೇವೆ ಸಲ್ಲಿಸಲು ಪ್ರೇರೇಪಿಸುತ್ತದೆ. \nನೀವು ಚಾಲಕರ ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ನಮೂದಿಸುವ ಮೂಲಕ ಸಹಾಯ ಮಾಡಬಹುದು ಮತ್ತು ನಮ್ಮ ಯಾತ್ರಿ ಸಮುದಾಯಕ್ಕಾಗಿ ಸವಾರಿಗಳ ಗುಣಮಟ್ಟವನ್ನು ಸುಧಾರಿಸಬಹುದು!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nನಿಮ್ಮ ನಮ್ಮ ಯಾತ್ರಿ ಚಾಲಕರನ್ನು ಕೇಳುವ ಮೂಲಕ ನೀವು ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ಪಡೆಯಬಹುದು.",
  "REFERRAL_CODE_SUCCESSFULL": "ನೀವು ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಅನ್ವಯಿಸಿರುವಿರಿ!",
  "REFERRAL_CODE_APPLIED": "ರೆಫರಲ್ ಅನ್ವಯಿಸಲಾಗಿದೆ!",
  "HEY": "ಹಾಯ್",
  "INVALID_CODE_PLEASE_RE_ENTER": "ಅಮಾನ್ಯ ಕೋಡ್. ದಯವಿಟ್ಟು ಮರು-ನಮೂದಿಸಿ",
  "LET_TRY_THAT_AGAIN": "ಅದನ್ನು ಮತ್ತೊಮ್ಮೆ ಪ್ರಯತ್ನಿಸೋಣ...",
  "CONTACTS_SELECTED": "ಸಂಪರ್ಕಗಳು ಆಯ್ಕೆಮಾಡಲಾಗಿದೆ",
  "SELECT_CONTACTS": "ಸಂಪರ್ಕಗಳನ್ನು ಆಯ್ಕೆಮಾಡಿ",
  "CONFIRM_EMERGENCY_CONTACTS": "ಅಗತ್ಯವಿರುವ ಸಂಪರ್ಕಗಳನ್ನು ದೃಢಪಡಿಸಿ",
  "MAXIMUM_CONTACTS_LIMIT_REACHED": "ಗರಿಷ್ಠ ಸಂಪರ್ಕಗಳ ಪರಿಮಿತಿ ಮುಗಿದಿದೆ",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT": "ನಿಮ್ಮ ತುರ್ತು ಸಂಪರ್ಕಗಳಿಂದ ಅವರನ್ನು ತೆಗೆದುಹಾಕಲು ನೀವು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "SEARCH_CONTACTS": "ಸಂಪರ್ಕಗಳನ್ನು ಹುಡುಕಿ",
  "SELECTED_CONTACT_IS_INVALID": "ಆಯ್ಕೆಮಾಡಿದ ಸಂಪರ್ಕವು ಅಮಾನ್ಯವಾಗಿದೆ. ದಯವಿಟ್ಟು ಸಂಖ್ಯೆಯನ್ನು ಮರುಪರಿಶೀಲಿಸಿ ಮತ್ತು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ",
  "CALL_EMERGENCY_CONTACTS": "ತುರ್ತು ಸಂಪರ್ಕಗಳಿಗೆ ಕರೆ ಮಾಡಿ",
  "LIVE_STATS_DASHBOARD": "ಲೈವ್ ಸ್ಥಿತಿ ಡ್ಯಾಶ್‌ಬೋರ್ಡ್",
  "CHECK_OUT_LIVE_STATS": "ಲೈವ್ ಅಂಕಿಅಂಶಗಳನ್ನು ಪರಿಶೀಲಿಸಿ",
  "ADD_ANOTHER_CONTACT": "ಮತ್ತೊಂದು ಸಂಪರ್ಕವನ್ನು ಸೇರಿಸಿ",
  "EMERGENCY_CONTACS_ADDED_SUCCESSFULLY": "ತುರ್ತು ಸಂಪರ್ಕಗಳನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಸೇರಿಸಲಾಗಿದೆ",
  "NO_CONTACTS_FOUND_ON_DEVICE_TO_ADD": "ಸೇರಿಸಲು ಯಾವುದೇ ಸಂಪರ್ಕಗಳು ಯಾವ ಸಾಧನದಲ್ಲಿ ಕಂಡುಬಂದಿಲ್ಲ",
  "NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD": "ಸೇರಿಸಲು ಯಾವುದೇ ಸಂಪರ್ಕಗಳು ಯಾವ ಸಾಧನದಲ್ಲಿ ಕಂಡುಬಂದಿಲ್ಲ",
  "PERMISSION_DENIED": "ಅನುಮತಿ ನಿರಾಕರಿಸಲಾಗಿದೆ",
  "PERCENTAGE_OF_NOMINAL_FARE": "~ ಮೂಲ ದರದ 10%",
  "REQUEST_CALLBACK": "ಕಾಲ್‌ಬ್ಯಾಕ್ ವಿನಂತಿಸಿ",
  "CHOOSE_YOUR_RIDE": "ನಿಮ್ಮ ಸವಾರಿಯನ್ನು ಆರಿಸಿ",
  "BOARD_THE_FIRST_TAXI": "ಜಾತ್ರಿ ಸತಿ \nವಲಯದಿಂದ ಮೊದಲ ಟ್ಯಾಕ್ಸಿ ಹತ್ತಿ",
  "PAY_VIA_CASH_OR_UPI": "ನಗದು / UPI ಮೂಲಕ ಪಾವತಿಸಿ",
  "ECONOMICAL": "ಆರ್ಥಿಕ",
  "COMFY": "ಆರಾಮದಾಯಕ",
  "NAVIGATE": "ನ್ಯಾವಿಗೇಟ್",
  "GOVERNMENT_CHAGRES": "ಸರ್ಕಾರದ ಶುಲ್ಕಗಳು",
  "SERVICE_CHARGES": "ಸೇವೆಯ ಶುಲ್ಕ",
  "CONFIRM_AND_BOOK": "ದೃಢೀಕರಿಸಿ ಮತ್ತು ಬುಕ್ ಮಾಡಿ",
  "PEOPLE": "ಜನರು",
  "SEARCH_AGAIN_WITH_A_TIP": "ಸಲಹೆಯೊಂದಿಗೆ ಮತ್ತೆ ಹುಡುಕುವುದೇ?",
  "TRY_AGAIN_WITH_A_TIP": "ಸಲಹೆಯೊಂದಿಗೆ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸುವುದೇ?",
  "BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS": "ಟಿಪ್ ಸೆ ರೈಡ್ ಮಿಲನೆ ಪೀಕ್ ಆವರ್ಸ್ ಕೆ ದೌರನ್ ಡ್ರೈವರ್ ಕಿ ಮದದ ಕರತಾ ಹೈ / ಖಾಲಿ ರಿಟರ್ನ್ ಟ್ರಿಪ್ ಕಿ ಭಾರತೈ",
  "TRY_AGAIN_WITHOUT_TIP": "ಸಲಹೆ ಇಲ್ಲದೆ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ",
  "SEARCH_AGAIN_WITHOUT_A_TIP": "ಸುಳಿವು ಇಲ್ಲದೆ ಮತ್ತೆ ಹುಡುಕಿ",
  "TRY_AGAIN_WITH": "ಇದರೊಂದಿಗೆ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ",
  "SEARCH_AGAIN_WITH": "ಇದರೊಂದಿಗೆ ಮತ್ತೆ ಹುಡುಕಿ",
  "TIP": "ತುದಿ",
  "START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS": "ಈ ತ್ವರಿತ ಚಾಟ್ ಸಲಹೆಗಳನ್ನು ಬಳಸಿಕೊಂಡು ನಿಮ್ಮ ಚಾಟ್ ಅನ್ನು ಪ್ರಾರಂಭಿಸಿ",
  "MESSAGE": "ಸಂದೇಶ",
  "START_YOUR_CHAT_WITH_THE_DRIVER": "ಚಾಲಕನೊಂದಿಗೆ ನಿಮ್ಮ ಚಾಟ್ ಅನ್ನು ಪ್ರಾರಂಭಿಸಿ",
  "I_AM_ON_MY_WAY": "ನಾನು ಬರುತ್ತಿರುವೆ",
  "GETTING_DELAYED_PLEASE_WAIT": "ತಡವಾಗುತ್ತಿದೆ, ನಿರೀಕ್ಷಿಸಿ",
  "UNREACHABLE_PLEASE_CALL_BACK": "ನನಗೆ ಕರೆ ಮಾಡಿ",
  "ARE_YOU_STARING": "ನೀವು ಪ್ರಾರಂಭಿಸುತ್ತಿದ್ದೀರಾ?",
  "PLEASE_COME_SOON": "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ",
  "OK_I_WILL_WAIT": "ಸರಿ, ನಾನು ಕಾಯುತ್ತೇನೆ",
  "I_HAVE_ARRIVED": "ನಾನು ಆಗಮಿಸಿದೆ",
  "PLEASE_COME_FAST_I_AM_WAITING": "ಬೇಗ ಬನ್ನಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ",
  "PLEASE_WAIT_I_WILL_BE_THERE": "ನಾನು ಬರುತ್ತಿದ್ದೇನೆ",
  "LOOKING_FOR_YOU_AT_PICKUP": "ನಾನು ಪಿಕಪ್‌ನಲ್ಲಿದ್ದೇನೆ",
  "MOBILE": "ಮೊಬೈಲ್",
  "HOW_DO_YOU_IDENTIFY_YOURSELF": "ನಿಮ್ಮನ್ನು ನೀವು ಹೇಗೆ ಗುರುತಿಸಿಕೊಳ್ಳುತ್ತೀರಿ?",
  "SELECT_YOUR_GENDER": "ನಿಮ್ಮ ಲಿಂಗವನ್ನು ಆಯ್ಕೆಮಾಡಿ",
  "FEMALE": "ಹೆಣ್ಣು",
  "MALE": "ಪುರುಷ",
  "PREFER_NOT_TO_SAY": "ಹೇಳದಿರಲು ಆದ್ಯತೆ",
  "EMAIL_ID": "ಇಮೇಲ್ ಐಡಿ",
  "SET_NOW": "ಈಗ ಅಂದಾಜು",
  "ADD_NOW": "ಈಗ ಸೇರಿಸಿ",
  "HOW_SHOULD_WE_ADDRESS_YOU": "ನಾವು ನಿಮ್ಮನ್ನು ಹೇಗೆ ಸಂಬೋಧಿಸಬೇಕು?",
  "GENDER_STR": "ಲಿಂಗ",
  "PROFILE_COMPLETION": "ಪ್ರೊಫೈಲ್ ಪೂರ್ಣಗೊಳಿಸುವಿಕೆ",
  "EARLY_END_RIDE_CHARGES": "ಬೇಗನೆ ಅಂತಿಮಗೊಂಡ ಸವಾರಿ ಶುಲ್ಕಗಳು^",
  "EARLY_END_RIDE_CHARGES_DESCRIPTION": "^ಮುಂಚಿತವಾಗಿ ಸವಾರಿ ಮಾಡುವುದು ಪ್ರಯಾಣಿಸದ ದೂರದ ಅರ್ಧದಷ್ಟು ದರದ ಹೆಚ್ಚುವರಿ ಶುಲ್ಕವನ್ನು ಉಂಟುಮಾಡುತ್ತದೆ (ಗರಿಷ್ಠ ₹ 50)",
  "CANCEL_ONGOING_SEARCH": "ನಡೆಯುತ್ತಿರುವ ಹುಡುಕಾಟವನ್ನು ರದ್ದುಗೊಳಿಸುವುದನ್ನು ಮುಂದುವರಿಸಲು ನೀವು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?",
  "YES_TRY_AGAIN": "ಹೌದು, ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ",
  "NO_DONT": "ಇಲ್ಲ, ಬೇಡ",
  "YES_CANCEL_SEARCH": "ಹೌದು, ಹುಡುಕಾಟ ರದ್ದುಮಾಡಿ",
  "TRY_LOOKING_FOR_RIDES_AGAIN": "ಇದು ತುಂಬಾ ಬಿಡುವಿಲ್ಲದ ದಿನವಾಗಿದೆ ಎಂದು ತೋರುತ್ತದೆ. ನೀವು ಮತ್ತೆ ಸವಾರಿಗಳನ್ನು ಹುಡುಕಲು ಪ್ರಯತ್ನಿಸಬಹುದು",
  "NO_TIP": "ಯಾವುದೇ ಸಲಹೆ ಇಲ್ಲ",
  "CUSTOMER_SELECTED_FARE": "ಗ್ರಾಹಕ ಸಲಹೆ^",
  "CUSTOMER_TIP_DESCRIPTION": "^ರೈಡ್ ಪಡೆಯುವ ಸಾಧ್ಯತೆಗಳನ್ನು ಹೆಚ್ಚಿಸಲು ಗ್ರಾಹಕರು ಹೆಚ್ಚುವರಿ ಮೊತ್ತವನ್ನು ಸೇರಿಸಿದ್ದಾರೆ.",
  "PLACE_CALL": "ಕರೆ ಮಾಡಿ",
  "DIRECT_CALL": "ನೇರ ಕರೆ",
  "ANONYMOUS_CALL": "ಅನಾಮಧೇಯ ಕರೆ",
  "YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE": "ನಿಮ್ಮ ಸಂಖ್ಯೆಯನ್ನು ಚಾಲಕನಿಗೆ ತೋರಿಸಲಾಗುವುದಿಲ್ಲ. ಅನುಸರಣೆಗಾಗಿ ಕರೆಯನ್ನು ರೆಕಾರ್ಡ್ ಮಾಡಲಾಗುತ್ತದೆ.",
  "YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER": "ನಿಮ್ಮ ಸಂಖ್ಯೆ ಚಾಲಕನಿಗೆ ಗೋಚರಿಸುತ್ತದೆ. ನೋಂದಾಯಿತ ಸಂಖ್ಯೆಯಿಂದ ಕರೆ ಮಾಡದಿದ್ದರೆ ಬಳಸಿ",
  "CALL_DRIVER_USING": "ಇದನ್ನು ಬಳಸಿಕೊಂಡು ಚಾಲಕಕ್ಕೆ ಕರೆ ಮಾಡಿ",
  "WAS_YOUR_CALL_SUCCESSFUL": "ನಿಮ್ಮ ಕರೆ ಯಶಸ್ವಿಯಾಗಿದೆಯೇ",
  "DRIVER_ADDITIONS": "ಚಾಲಕ ಸೇರ್ಪಡೆ*",
  "FARE_UPDATE_POLICY": "ದರ ನವೀಕರಣ ನೀತಿ",
  "DRIVER_ADDITIONS_OPTIONAL": "ಚಾಲಕ ಸೇರ್ಪಡೆ (ಐಚ್ಛಿಕ)",
  "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "ಟ್ರಾಫಿಕ್, ರಿಟರ್ನ್ ಟ್ರಿಪ್‌ನ ಅವಕಾಶ ಇತ್ಯಾದಿಗಳನ್ನು ಸರಿದೂಗಿಸಲು ಚಾಲಕ ಹೆಚ್ಚುವರಿಯನ್ನು ಉಲ್ಲೇಖಿಸಬಹುದು",
  "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ಚಾಲಕ ಸೇರ್ಪಡೆ ಮಿತಿಯನ್ನು ಮೂಲ ದರದ 10% ರಷ್ಟನ್ನು ಸುಮಾರು ₹10 ಕ್ಕೆ ಲೆಕ್ಕಹಾಕಲಾಗುತ್ತದೆ",
  "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE": "ಗಮನಿಸಿ: ಚಾಲಕರು ಈ ಹೆಚ್ಚುವರಿ ದರವನ್ನು ವಿಧಿಸಬಹುದು/ಇಲ್ಲ.",
  "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS": "ಕೆಳಗಿನ ಯಾವುದೇ ಕಾರಣಗಳಿಂದ ನೀವು ನವೀಕರಿಸಿದ ಅಂತಿಮ ದರವನ್ನು ನೋಡಬಹುದು:",
  "REASON_CHANGE_IN_ROUTE_A": "1.ಮಾರ್ಗದಲ್ಲಿ ಬದಲಾವಣೆ: ",
  "REASON_CHANGE_IN_ROUTE_B": "ಮಾರ್ಗದಲ್ಲಿನ ಬದಲಾವಣೆಯಿಂದಾಗಿ ಒಟ್ಟು ದರ ಬದಲಾಗಬಹುದು",
  "NAVIGATE_TO_PICKUP" : "ಪಿಕಪ್ ಮಾಡಲು ನ್ಯಾವಿಗೇಟ್ ಮಾಡಿ",
  "REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON" : "ವಿನಂತಿಯನ್ನು ಸ್ವೀಕರಿಸಲಾಗಿದೆ. ನಾವು ಶೀಘ್ರದಲ್ಲೇ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತೇವೆ",
  "CONTACT_REMOVED_SUCCESSFULLY" : "ಸಂಪರ್ಕವನ್ನು ಯಶಸ್ವಿಯಾಗಿ ತೆಗೆದುಹಾಕಲಾಗಿದೆ",
  "CORPORATE_ADDRESS" : "ಕಾರ್ಪೊರೇಟ್ ವಿಳಾಸ",
  "CORPORATE_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Girija Building, Number 817, Ganapathi Temple Rd, 8th Block, Koramangala, Bengaluru, Karnataka 560095, India.",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
  "REGISTERED_ADDRESS" : "ನೋಂದಾಯಿಸಿದ ವಿಳಾಸ",
  "REGISTERED_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala Bengaluru, Karnataka- 560095, India.",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
  "RECOMMENDED" : "ಶಿಫಾರಸು ಮಾಡಲಾಗಿದೆ",
  "WE_WOULD_APPRECIATE_YOUR_FEEDBACK" : "ಖಾತೆ ಅಳಿಸುವಿಕೆಯ ಹಿಂದಿನ ನಿಮ್ಮ ತಾರ್ಕಿಕತೆಯನ್ನು ನಾವು ಪ್ರಶಂಸಿಸುತ್ತೇವೆ",
  "REASON_FOR_DELETING_ACCOUNT" : "ಖಾತೆಯನ್ನು ಅಳಿಸಲು ಕಾರಣ*",
  "SUBMIT_REQUEST" : "ವಿನಂತಿಯನ್ನು ಸಲ್ಲಿಸಿ",
  "PLEASE_ENTER_A_VALID_EMAIL" : "ದಯವಿಟ್ಟು ಮಾನ್ಯವಾದ ಇಮೇಲ್ ಅನ್ನು ನಮೂದಿಸಿ"
}