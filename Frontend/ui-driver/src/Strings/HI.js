
export function getStringValue(key) {
  if (key in hindiStrings) {
    return hindiStrings[key];
  }
  console.error( key + " not found in hindiStrings");
  return "";
}

const hindiStrings = {
  LETS_GET_STARTED: "चलिए शुरू करते हैं",
  YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION: "आपका आवेदन सफलतापूर्वक जमा कर दिया गया है और सत्यापन के अधीन है",
  VIEW_STATUS: "स्थिति देखें",
  GO_HOME: "गो होम",
  SELECT_LANGUAGE: "भाषा चुनें",
  WHICH_LANGUAGE_DO_YOU_PREFER: "आप कौन सी भाषा पसंद करते हैं",
  T_C: "उपयोग की शर्ते प्राइवेसि पॉलिसी",
  ENTER_MOBILE_NUMBER: "मोबाइल नंबर दर्ज करें",
  BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR: "जारी रखने पर क्लिक करके\nए) आप इस बात से सहमत हैं कि आप बीटा परीक्षण के इच्छुक प्रतिभागी हैं और जुसपे का किसी भी संबंध में आपके खिलाफ कोई दायित्व नहीं होगा",
  ENTER_OTP: "ओटीपी दर्ज करें",
  DIDNT_RECIEVE_OTP: "ओटीपी प्राप्त नहीं हुआ ?",
  RESEND_OTP: "ओटीपी पुनः भेजें",
  PLEASE_ENTER_VALID_OTP: "कृपया सही OTP दर्ज करें",
  INVALID_MOBILE_NUMBER: "गलत मोबाइल नंबर",
  REGISTER: "रजिस्टर करें",
  MOBILE_NUMBER: "मोबाइल नंबर",
  AUTO_READING_OTP: "ऑटो रीडिंग ओटीपी ...",
  UPLOAD_DRIVING_LICENSE: "ड्राइविंग लाइसेंस अपलोड करें",
  UPLOAD_BACK_SIDE: "पिछला भाग अपलोड करें",
  UPLOAD_FRONT_SIDE: "अपने DL का फोटो साइड अपलोड करें",
  BACK_SIDE: "पिछला भाग",
  FRONT_SIDE: "आपके DL का फोटो साइड",
  NEXT: "अगला पृष्ठ",
  LICENSE_INSTRUCTION_PICTURE: "कृपया लाइसेंस के दोनों पक्षों की स्पष्ट तस्वीरें अपलोड करें",
  LICENSE_INSTRUCTION_CLARITY: "सुनिश्चित करें कि फोटो और सभी विवरण स्पष्ट रूप से दिखाई दे रहे हैं",
  REGISTRATION_STEPS: "पंजीकरण चरण",
  PROGRESS_SAVED: "आपकी प्रगति सहेज ली गई है, आप किसी भी जानकारी को बदलने के लिए पिछले चरणों पर वापस जा सकते हैं",
  DRIVING_LICENSE: "ड्राइविंग लाइसेंस",
  AADHAR_CARD: "आधार कार्ड",
  BANK_DETAILS: "बैंक विवरण",
  VEHICLE_DETAILS: "वाहन की सूचना",
  UPLOAD_FRONT_BACK: "आगे और पीछे के भाग अपलोड करें",
  EARNINGS_WILL_BE_CREDITED: "आपकी कमाई यहां क्रेडिट की जाएगी",
  FILL_VEHICLE_DETAILS: "अपने वाहन का विवरण भरें",
  FOLLOW_STEPS: "रजिस्टर करने के लिए कृपया नीचे दिए गए चरणों का पालन करें",
  REGISTRATION: "पंजीकरण",
  UPLOAD_ADHAAR_CARD: "आधार कार्ड अपलोड करें",
  ADHAAR_INTRUCTION_PICTURE: "कृपया आधार कार्ड के दोनों पक्षों की स्पष्ट तस्वीरें अपलोड करें",
  ADD_VEHICLE_DETAILS: "वाहन विवरण जोड़ें",
  VEHICLE_REGISTRATION_NUMBER: "वाहन पंजीकरण संख्या",
  RE_ENTER_VEHICLE_REGISTRATION_NUMBER: "फिर से वाहन पंजीकरण संख्या",
  ENTER_VEHICLE_NO: "वाहन संख्या दर्ज करें।",
  VEHICLE_TYPE: "वाहन का प्रकार",
  VEHICLE_MODEL_NAME: "वाहन मॉडल का नाम",
  ENTER_MODEL_NAME: "मॉडल का नाम दर्ज करें",
  VEHICLE_COLOUR: "वाहन का रंग",
  ENTER_VEHICLE_COLOUR: "वाहन का रंग दर्ज करें",
  UPLOAD_REGISTRATION_CERTIFICATE: "पंजीकरण प्रमाणपत्र अपलोड करें (आरसी)",
  UPLOAD_RC: "आरसी अपलोड करें",
  PREVIEW: "इमेज देखें",
  CHOOSE_VEHICLE_TYPE: "वाहन का प्रकार चुनें",
  BENIFICIARY_NUMBER: "लाभार्थी खाता नंबर",
  RE_ENTER_BENIFICIARY_NUMBER: "लाभार्थी खाता संख्या पुनः दर्ज करें।",
  IFSC_CODE: "IFSC कोड",
  SENDING_OTP: "ओटीपी भेज रहे है",
  PLEASE_WAIT_WHILE_IN_PROGRESS: "कृपया प्रगति के दौरान प्रतीक्षा करें",
  LIMIT_EXCEEDED: "सीमा पार हो गई",
  YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN: "आपके अनुरोध का समय समाप्त हो गया है फिर से प्रयास करें",
  ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER: "त्रुटि हुई कृपया बाद में पुन: प्रयास करें",
  LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN: "सीमा पार हो गई कृपया बाद में पुन: प्रयास करें",
  ENTER_OTP_SENT_TO: " पर भेजे गए OTP को दर्ज करें",
  OTP_SENT_TO: " पर OTP भेजा गया",
  COUNTRY_CODE_INDIA: "+91",
  ENTER_ACCOUNT_NUMBER: "खाता संख्या दर्ज करें।",
  ADD_BANK_DETAILS: "बैंक विवरण जोड़ें",
  ENTER_IFSC_CODE: "IFSC कोड दर्ज करें",
  SUBMIT: "जमा करें",
  PERSONAL_DETAILS: "व्यक्तिगत विवरण",
  LANGUAGES: "बोली",
  HELP_AND_FAQ: "सहायता और अक्सर पूछे जाने वाले प्रश्न",
  ABOUT: "एप के बारे में",
  LOGOUT: "लॉग आउट",
  UPDATE: "अपडेट करें",
  EDIT: "संपादन करें",
  AUTO: "ऑटो",
  NAME: "नाम",
  PRIVACY_POLICY: "गोपनीयता नीति",
  LOGO: "प्रतीक चिन्ह",
  ABOUT_APP_DESCRIPTION: "यात्रियों के साथ ड्राइवरों को जोड़ने के लिए यात्री पार्टनर एक खुला मंच है। ऐप ड्राइवरों के लिए यात्रियों को ढूंढना आसान बनाता है। और सेवा प्रदाताओं के साथ जोड़कर इन विकल्पों का लाभ उठाएं",
  TERMS_AND_CONDITIONS: "नियम एवं शर्तें",
  UPDATE_VEHICLE_DETAILS: "वाहन विवरण अपडेट करें",
  Help_AND_SUPPORT: "सहायता और समर्थन",
  NOTE: "टिप्पणी:",
  VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS: "विशिष्ट शिकायतों के लिए माई राइड्स अनुभाग पर जाएँ",
  THANK_YOU_FOR_WRTITTING_US: "हमें लिखने के लिए धन्यवाद!",
  WE_HAVE_RECIEVED_YOUR_ISSUE: "हमें आपका मुद्दा मिल गया है। हम कुछ देर में आपसे संपर्क करेंगे",
  GO_TO_HOME: "गो होम",
  YOUR_RECENT_RIDE: "आपकी हाल की सवारी",
  YOUR_RECENT_TRIP: "Your Recent Trip",
  ALL_TOPICS: "सभी विषय",
  REPORT_AN_ISSUE_WITH_THIS_TRIP: "इस यात्रा में किसी समस्या की रिपोर्ट करें",
  YOU_RATED: "आपने मूल्यांकन किया",
  VIEW_ALL_RIDES: "सभी सवारी देखें",
  WRITE_TO_US: "हमे संपर्क करे",
  SUBJECT: "विषय",
  YOUR_EMAIL_ID: "आपकी ईमेल आईडी",
  DESCRIBE_YOUR_ISSUE: "अपनी समस्या का वर्णन करें",
  GETTING_STARTED_AND_FAQ: "आरंभ करना और अक्सर पूछे जाने वाले प्रश्न",
  FOR_OTHER_ISSUES_WRITE_TO_US: "अन्य मुद्दों के लिए, हमें लिखें",
  CALL_SUPPORT_CENTER: "कॉल सपोर्ट सेंटर",
  YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE: "आप यहां जिस समस्या का सामना कर रहे हैं उसका वर्णन कर सकते हैं",
  REGISTRATION_CERTIFICATE_IMAGE: "पंजीकरण प्रमाणपत्र (आरसी) छवि",
  HOME: "होम",
  RIDES: "सवारी",
	TRIPS: "ट्रिप्स",
  PROFILE: "प्रोफ़ाइल",
  ENTER_DRIVING_LICENSE_NUMBER: "ड्राइविंग लाइसेंस नंबर दर्ज करें",
  WHERE_IS_MY_LICENSE_NUMBER: "मेरा लाइसेंस नंबर कहां है?",
  TRIP_DETAILS: "यात्रा विवरण",
  BY_CASH: "नकद द्वारा",
  ONLINE_: "ऑनलाइन",
  REPORT_AN_ISSUE: "मामले की रिपोर्ट करें",
  DISTANCE: "दूरी",
  TIME_TAKEN: "समय लिया",
  OPEN_GOOGLE_MAPS: "गूगल मैप्स खोलें",
  CALL: "कॉल",
  START_RIDE: "सवारी शुरू करें",
  CANCEL_RIDE: "सवारी रद्द करें",
  END_RIDE: "सवारी अंत करें",
  RIDE_COMPLETED_WITH: "सवारी पूरी हुई",
  COLLECT_AMOUNT_IN_CASH: "नकद में राशि जमा करें",
  CASH_COLLECTED: "नकद एकत्रित",
  OFFLINE: "ऑफलाइन",
  ACCEPT_FOR: "इसके लिए स्वीकार करें:",
  DECLINE: "रद्द करे",
  REQUEST: "रीक्वेस्ट",
  YOU_ARE_OFFLINE: "आप ऑफ़लाइन हैं",
  YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS: "आप इस समय व्यस्त हैं। यात्रा अनुरोध प्राप्त करने के लिए ऑनलाइन जाएं",
  GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE: "ऑफलाइन जाने से आपको कोई सवारी नहीं मिलेगी",
  CANCEL: "रद्द करें",
  GO_OFFLINE: "ऑफ़ लाइन हो जाओ",
  IS_WAITING_FOR_YOU: "पिकअप करने जा रहे हैं",
  YOU_ARE_ON_A_RIDE: "सवारी शुरू हुई...",
  PLEASE_ASK_RIDER_FOR_THE_OTP: "कृपया ओटीपी के लिए राइडर से पूछें",
  COMPLETED_: "पूरा हुआ",
  CANCELLED_: "रद्द",
  ENTER_RC_NUMBER: "आरसी नंबर दर्ज करें",
  ALLOW_ACCESS: "उपयोग की अनुमति दें",
  WE_NEED_SOME_ACCESS: "हमें कुछ ऐक्सेस चाहिए",
  WHERE_IS_MY_RC_NUMBER: "मेरा आरसी नंबर कहां है?",
  THANK_YOU_FOR_WRITING_TO_US: "हमें लिखने के लिए धन्यवाद!",
  RIDER: "सवार",
  TRIP_ID: "ट्रिप आईडी",
  NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST: "ऐप बंद होने पर आपको आने वाली सवारी अनुरोध दिखाने की आवश्यकता है",
  NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP: "ऐप के लिए बैटरी ऑप्टिमाइज़ेशन को अक्षम करने के लिए, ऐप बंद होने पर आपको ऑनलाइन रखने की आवश्यकता है",
  NEED_IT_TO_AUTOSTART_YOUR_APP: "ऐप बंद होने पर आपको ऑनलाइन रखने के लिए अपने ऐप को ऑटोस्टार्ट करने की आवश्यकता है",
  NEED_IT_TO_ENABLE_LOCATION: "नम्मा यात्री पार्टनर ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
  OVERLAY_TO_DRAW_OVER_APPLICATIONS: "अनुप्रयोगों पर आकर्षित करने के लिए ओवरले",
  BATTERY_OPTIMIZATIONS: "बैटरी अनुकूलन",
  AUTO_START_APPLICATION_IN_BACKGROUND: "पृष्ठभूमि में ऑटोस्टार्ट आवेदन",
  LOCATION_ACCESS: "स्थान अभिगम",
  STEP: "कदम",
  PAID: "प्रदत्त",
  PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL: "कृपया हमें बताएं कि आप क्यों रद्द करना चाहते हैं",
  MANDATORY: "अनिवार्य",
  ENTERED_WRONG_OTP: "गलत ओटीपी दर्ज किया",
  COPIED: "कॉपी किया गया",
  BANK_NAME: "बैंक का नाम",
  AADHAR_DETAILS: "आधार विवरण",
  AADHAR_NUMBER: "आधार नंबर",
  FRONT_SIDE_IMAGE: "सामने की ओर छवि",
  BACK_SIDE_IMAGE: " बैक साइड इमेज",
  STILL_NOT_RESOLVED: "अभी भी हल नहीं है? हमें बुलाओ",
  CASE_TWO: "ब) ",
  NON_DISCLOUSER_AGREEMENT: "कोई अस्वीकरण समझौता नहीं",
  DATA_COLLECTION_AUTHORITY: "सी) मैं अपनी जानकारी एकत्र करने के लिए Juspay को नियुक्त और अधिकृत करता हूं, और जारी रखने से, मैं उपयोग की शर्तों और गोपनीयता नीति से सहमत हूं।",
  SOFTWARE_LICENSE: "सॉफ़्टवेयर लाइसेंस",
  LOAD_MORE: "और लोड करें",
  ARE_YOU_SURE_YOU_WANT_TO_LOGOUT: "क्या आप लॉग आउट करना चाहते हैं?",
  GO_BACK: "पीछे जाएं",
  ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE: "क्या आप वाकई सवारी खत्म करना चाहते हैं?",
  THANK_YOU_FOR_REGISTERING_US: "हमारे साथ रजिस्टर के लिए धन्यवाद!",
  UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU: "दुर्भाग्य से, हम आपके लिए अभी तक उपलब्ध नहीं हैं। हम आपको जल्द ही सूचित करेंगे।",
  EMPTY_RIDES: "खाली सवारी",
  YOU_HAVE_NOT_TAKEN_A_TRIP_YET: "आपने अभी तक कोई यात्रा नहीं की है",
  BOOK_NOW: "अभी बुक करें",
  RESEND_OTP_IN: "ओटीपी फिर से भेजें",
  WE_NEED_ACCESS_TO_YOUR_LOCATION: "हमें आपके लोकेशन की आवश्यकता है!",
  YOUR_LOCATION_HELPS_OUR_SYSTEM: "आपका लोकेशन हमारे सिस्टम को आस-पास के सभी ऑटो को मैप करने में मदद करता है और आपको सबसे तेज सवारी प्रदान करता है।",
  NO_INTERNET_CONNECTION: "कोई इंटरनेट कनेक्शन नहीं",
  PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN: "कृपया अपना इंटरनेट कनेक्शन जांचें और पुनः प्रयास करें",
  TRY_AGAIN: "पुन: प्रयास करें",
  GRANT_ACCESS: " एक्सेस प्रदान करे",
  YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN: "आप सीमा पार कर गए हैं, 10 मिनट के बाद पुन: प्रयास करें",
  ENTER_REFERRAL_MOBILE_NUMBER: "रेफ़रल मोबाइल नंबर दर्ज करें",
  APPLY: "लागू",
  HAVE_A_REFERRAL: "एक रेफरल है?",
  ADD_HERE: "यहां जोड़ें",
  REFERRAL_APPLIED: "रेफ़रल लागू!",
  SMALLEDIT: "संपादन करें",
  ADD_DRIVING_LICENSE: "ड्राइविंग लाइसेंस जोड़ें",
  HELP: "सहायता?",
  INVALID_DL_NUMBER: "गलत ड्राइविंग लाइसेंस नंबर",
  DRIVING_LICENSE_NUMBER: "ड्राइविंग लाइसेंस नंबर",
  RE_ENTER_DRIVING_LICENSE_NUMBER: "फिर से ड्राइविंग लाइसेंस नंबर",
  ENTER_DL_NUMBER: "ड्राइविंग लाइसेंस नंबर दर्ज करें",
  SELECT_DATE_OF_BIRTH: "जन्म की तारीख चुनें",
  DATE_OF_BIRTH: "जन्म की तारीख",
  WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION: "आसान पंजीकरण के लिए एक ट्यूटोरियल देखें",
  ENTER_MINIMUM_FIFTEEN_CHARACTERS: "न्यूनतम 15 वर्ण दर्ज करें",
  ADD_YOUR_FRIEND: "अपने दोस्त को जोड़ें",
  PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE: "कृपया छवि की पुष्टि करते हुए प्रतीक्षा करें",
  VALIDATING: "पुष्टि जारी",
  VERIFICATION_PENDING: "जाँच जारी",
  VERIFICATION_FAILED: "जाँच असफल",
  NO_DOC_AVAILABLE: "कोई दस्तावेज उपलब्ध नहीं है",
  ISSUE_WITH_DL_IMAGE: "ऐसा लगता है कि आपकी DL इमेज में कुछ समस्या है, हमारी सहायता टीम जल्द ही आपसे संपर्क करेगी।",
  STILL_HAVE_SOME_DOUBT: "अभी भी कुछ संदेह है?",
  ISSUE_WITH_RC_IMAGE: "ऐसा लगता है कि आपकी RC इमेज में कुछ समस्या है, हमारी सहायता टीम जल्द ही आपसे संपर्क करेगी।",
  PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT: "कृपया छवि के लिए जाँच करें कि वैध दस्तावेज़ छवि है या नहीं",
  OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED: "ओह! आपका आवेदन खारिज कर दिया गया है। कृपया पुन: प्रयास करें",
  INVALID_DRIVING_LICENSE: "अमान्य ड्राइविंग लाइसेंस",
  LIMIT_EXCEEDED_FOR_DL_UPLOAD: "आरसी अपलोड के लिए सीमा पार",
  INVALID_VEHICLE_REGISTRATION_CERTIFICATE: "अमान्य वाहन RC",
  LIMIT_EXCEEDED_FOR_RC_UPLOAD: "आरसी अपलोड के लिए सीमा पार",
  YOUR_DOCUMENTS_ARE_APPROVED: "आपके दस्तावेज़ स्वीकृत हैं। सहायता टीम आपके खाते को जल्द ही सक्षम कर देगी। आप अपना खाता सक्षम करने के लिए सहायता टीम को भी कॉल कर सकते हैं",
  APPLICATION_STATUS: "आवेदन की स्थिति",
  FOR_SUPPORT: "समर्थन के लिए",
  CONTACT_US: " संपर्क करें",
  IMAGE_VALIDATION_FAILED: "छवि का सत्यापन विफल रहा",
  IMAGE_NOT_READABLE: "छवि पढ़ने योग्य नहीं है",
  IMAGE_LOW_QUALITY: "छवि गुणवत्ता अच्छी नहीं है",
  IMAGE_INVALID_TYPE: "बशर्ते छवि प्रकार वास्तविक प्रकार से मेल नहीं खाता",
  IMAGE_DOCUMENT_NUMBER_MISMATCH: "इस छवि में दस्तावेज़ संख्या इनपुट के साथ मेल नहीं खा रही है",
  IMAGE_EXTRACTION_FAILED: "छवि निष्कर्षण विफल",
  IMAGE_NOT_FOUND: "छवि नहीं मिली",
  IMAGE_NOT_VALID: "छवि मान्य नहीं है",
  DRIVER_ALREADY_LINKED: "अन्य दस्तावेज़ पहले से ही ड्राइवर के साथ जुड़ा हुआ है",
  DL_ALREADY_UPDATED: "कोई कार्रवाई आवश्यक नहीं। ड्राइवर का लाइसेंस पहले से ही ड्राइवर से जुड़ा हुआ है",
  RC_ALREADY_LINKED: "वाहन RC उपलब्ध नहीं है। अन्य ड्राइवर से जुड़ा",
  RC_ALREADY_UPDATED: "कोई कार्रवाई आवश्यक नहीं। वाहन आरसी पहले से ही चालक से जुड़ा हुआ है",
  DL_ALREADY_LINKED: "चालक लाइसेंस उपलब्ध नहीं है। अन्य ड्राइवर से जुड़ा",
  SOMETHING_WENT_WRONG: "कुछ गलत हो गया",
  PICKUP: "उठाना",
  TRIP: "यात्रा",
  CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER: "वर्तमान में, हम केवल कर्नाटक पंजीकृत संख्या की अनुमति देते हैं",
  UPDATED_AT: "अपडेट हुआ",
  DATE_OF_REGISTRATION: "पंजीकरण की तिथि",
  SELECT_DATE_OF_ISSUE: "जारी करने की तिथि चुनें",
  DATE_OF_ISSUE: "जारी करने की तिथि",
  PROVIDE_DATE_OF_ISSUE_TEXT: "क्षमा करें, हम आपके DL को सत्यापित(verify) नहीं कर सके, कृपया अपने DL को सत्यापित(verify) करने के लिए <b> जारी करने की तिथि चुनें(Date of Issue) </b> प्रदान करें",
  PROVIDE_DATE_OF_REGISTRATION_TEXT: "क्षमा करें, हम आपके RC को सत्यापित(verify) नहीं कर सके, कृपया अपने RC को सत्यापित(verify) करने के लिए <b> पंजीकरण की तिथि(Date of Registration) </b> प्रदान करें",
  SELECT_DATE_OF_REGISTRATION: "पंजीकरण की तिथि चुनें",
  TRIP_COUNT: "आज की यात्राएं",
  TODAYS_EARNINGS: "आज की कमाई",
  SAME_REENTERED_RC_MESSAGE: "कृपया सुनिश्चित करें कि फिर से दर्ज किया गया आरसी नंबर ऊपर दिए गए आरसी नंबर के समान है",
  SAME_REENTERED_DL_MESSAGE: "पुनः दर्ज किया गया डीएल नंबर ऊपर दिए गए डीएल नंबर से मेल नहीं खाता है",
  WHERE_IS_MY_ISSUE_DATE: "जारी करने की तिथि कहाँ है?",
  WHERE_IS_MY_REGISTRATION_DATE: "पंजीकरण तिथि कहां है?",
  OTP_RESENT: "ओटीपी फिर से भेजा है",
  EARNINGS_CREDITED_IN_ACCOUNT: "आपकी कमाई इस खाते में जमा की जाएगी",
  INVALID_PARAMETERS: "अमान्य मापदण्ड",
  UNAUTHORIZED: "अनधिकृत",
  INVALID_TOKEN: "अमान्य टोकन",
  SOME_ERROR_OCCURED_IN_OFFERRIDE: "ऑफ़रराइड में कुछ त्रुटि हुई",
  SELECT_VEHICLE_TYPE: "वाहन का प्रकार चुनें",
  RIDE: "सवारी",
  NO_LOCATION_UPDATE: "कोई स्थान अद्यतन नहीं",
  GOT_IT_TELL_US_MORE: "समझ गए, हमें और बताएं?",
  WRITE_A_COMMENT: "टिप्पणी लिखें",
  HOW_WAS_YOUR_RIDE_WITH: "आपकी राइड कैसी रही",
  RUDE_BEHAVIOUR: "अशिष्ट व्यवहार",
  LONG_WAITING_TIME: "लंबे समय तक प्रतीक्षा",
  DIDNT_COME_TO_PICUP_LOCATION: "पिकअप स्थान पर नहीं आया",
  HELP_US_WITH_YOUR_REASON: "अपने कारण से हमारी मदद करें",
  MAX_CHAR_LIMIT_REACHED: "अधिकतम वर्ण सीमा पूरी हुई,",
  SHOW_ALL_OPTIONS: "सभी विकल्प दिखाएं",
  UPDATE_REQUIRED: "अद्यतन आवश्यक है",
  PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE: "कृपया सेवा जारी रखने के लिए ऐप को अपडेट करें",
  NOT_NOW: "अभी नहीं",
  OF: "में से",
  DROP: "ड्रॉप",
  PLEASE_WAIT: "कृपया प्रतीक्षा करें",
  SETTING_YOU_OFFLINE: "हम आपको ऑफ़लाइन सेट कर रहे हैं",
  SETTING_YOU_ONLINE: "हम आपको ऑनलाइन सेट कर रहे हैं",
  SETTING_YOU_SILENT : "हम आपको साइलेंट सेट कर रहे हैं",
  VIEW_BREAKDOWN: "ब्रेकडाउन देखें",
  APP_INFO: "ऐप सेटिंग",
  OTHER: "अन्य",
  VEHICLE_ISSUE: "वाहन का मामला",
  FARE_UPDATED: "किराया अपडेट किया",
  FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES: "बार-बार रद्द करने से सवारी कम होगी और रेटिंग कम होगी",
  CONTINUE: "जारी रखें",
  CONFIRM_PASSWORD: "पासवर्ड की पुष्टि कीजिये",
  DEMO_MODE: "डेमो मोड",
  PASSWORD: "पासवर्ड",
  ENTER_DEMO_MODE_PASSWORD: "डेमो मोड पासवर्ड दर्ज करें",
  DEMO_MODE_DISABLED: "डेमो मोड अक्षम",
  ONLINE_VIA_DEMO_MODE: "ऑनलाइन (डेमो)",
  MORE: "more",
  LESS: "less",
  YOU_ARE_AT_PICKUP: "आप पिकअप स्थान पर हैं",
  WAITING_FOR_CUSTOMER: "",
  CUSTOMER_NOTIFIED: "ग्राहक अधिसूचित",
  I_ARRIVED: "मैं आ गया हूँ",
  ESTIMATED_RIDE_FARE: "अनुमानित सवारी किराया: ",
  PICKUP_TOO_FAR: "पिकअप बहुत दूर है",
  CUSTOMER_NOT_PICKING_CALL: "ग्राहक कॉल नहीं उठा रहा है",
  TRAFFIC_JAM: "ट्रैफ़िक जाम",
  CUSTOMER_WAS_RUDE: "ग्राहक बदतमीजी कर रहा था",
  ALL_MESSAGES: "सभी संदेश",
  MESSAGES: "संदेशे",
  ADD_A_COMMENT: "एक टिप्पणी जोड़ने",
  POST_COMMENT: "टिप्पणी करें",
  ENTER_YOUR_COMMENT: "अपनी टिप्पणी दर्ज करें",
  NO_NOTIFICATIONS_RIGHT_NOW: "अभी कोई सूचना नहीं है",
  NO_NOTIFICATIONS_RIGHT_NOW_DESC: "कोई नई सूचना आने पर हम आपको सूचित करेंगे",
  ALERTS: "अलर्टस",
  YOUR_COMMENT: "आपकी टिप्पणियां",
  SHOW_MORE: "अधिक दिखाएँ",
  LOAD_OLDER_ALERTS: "और लोड करें",
  CONTEST: "प्रतियोगिता",
  YOUR_REFERRAL_CODE_IS_LINKED: "आपका रेफरल कोड लिंक हो गया है!",
  YOU_CAN_NOW_EARN_REWARDS: "अब आप ग्राहकों को रेफ़र करने के लिए पुरस्कार अर्जित कर सकते हैं!",
  COMING_SOON: "जल्द आ रहा है!",
  COMING_SOON_DESCRIPTION: "हम आपको रेफ़रल कार्यक्रम में शामिल करने के लिए काम कर रहे हैं। अधिक जानकारी के लिए अलर्ट पेज देखें।",
  REFERRAL_CODE: "रेफरल कोड",
  REFERRAL_CODE_HINT: "6 अंकों का रेफरल कोड दर्ज करें",
  CONFIRM_REFERRAL_CODE: "रेफरल कोड की पुष्टि करें",
  CONFIRM_REFERRAL_CODE_HINT: "रेफरल कोड फिर से दर्ज करें",
  YOUR_REFERRAL_CODE: "आपका रेफरल कोड",
  FIRST_REFERRAL_SUCCESSFUL: "पहला रेफ़रल सफल!\nइनाम अनलॉक किया गया!",
  AWAITING_REFERRAL_RIDE: "रेफरल राइड का इंतजार है",
  CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT: "जब आप रेफ़रल अलर्ट प्राप्त करें तो \nइस स्थान को चेक करें",
  REFERRED_CUSTOMERS: "रेफ़र किए गए ग्राहक",
  ACTIVATED_CUSTOMERS: "सक्रिय ग्राहक",
  REFERRAL_CODE_LINKING: "रेफरल कोड लिंकिंग",
  CONTACT_SUPPORT: "संपर्क सहायता",
  CALL_SUPPORT: "कॉल सहायता",
  YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT: "आप नम्मा यात्री सपोर्ट टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
  REFERRAL_ENROLMENT: "रेफरल नामांकन",
  REFERRALS: "रेफरल",
  LINK_REFERRAL_CODE: "लिंक रेफरल कोड",
  DRIVER_DETAILS: "ड्राइवर का विवरण",
  FOR_UPDATES_SEE_ALERTS: "अपडेट के लिए, अलर्ट देखें",
  SHARE_OPTIONS: "विकल्प साझा करें",
  ENTER_PASSWORD: "पास वर्ड दर्ज करें",
  YOUR_VEHICLE: "आपका वाहन",
  BOOKING_OPTIONS: "बुकिंग विकल्प",
  CONFIRM_AND_CHANGE: "पुष्टि करें और बदलें",
  OTP: "OTP",
  RIDE_FARE : "सवारी किराया",
  RIDE_DISTANCE : "सवारी की दूरी",
  FARE_UPDATED : "किराया अपडेट किया",
  START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS : "इन त्वरित चैट सुझावों का उपयोग करके अपनी चैट प्रारंभ करें",
  MESSAGE : "संदेश",
  START_YOUR_CHAT_WITH_THE_DRIVER : "ड्राइवर के साथ अपनी चैट शुरू करें",
  I_AM_ON_MY_WAY : "मैं रास्ते में हूं|",
  GETTING_DELAYED_PLEASE_WAIT : "देरी हो रही है। प्रतीक्षा करें|",
  UNREACHABLE_PLEASE_CALL_BACK : "पहुंच योग्य नहीं है, वापस कॉल करें।",
  ARE_YOU_STARING : "क्या आप आ रहे हैं?",
  PLEASE_COME_SOON : "कृपया जल्दी आ जाएँ|",
  OK_I_WILL_WAIT : "मैं इंतज़ार करूंगा|",
  I_HAVE_ARRIVED : "मैं पहूंच गया हूं|",
  PLEASE_COME_FAST_I_AM_WAITING : "जल्दी आइए, मैं इंतजार कर रहा हूं।",
  PLEASE_WAIT_I_WILL_BE_THERE : "कृपया प्रतीक्षा करें, मैं आ रहा हूँ|",
  LOOKING_FOR_YOU_AT_PICKUP : "मैं पिकअप पर हूँ|",
  SILENT : "साइलेंट",
  TRY_SILENT_MODE : "ट्राइ साइलेंट मोड?",
  SILENT_MODE_PROMPT : "अगर आप परेशान नहीं होना चाहते हैं, तो आप इसके बजाय साइलेंट मोड में स्विच कर सकते हैं",
  GO_SILENT : "साइलेंट हो जाओ",
  GO_ONLINE : "गो!",
  GO_ONLINE_PROMPT : "आप वर्तमान में ऑफ़लाइन हैं।\nराइड अनुरोध प्राप्त करने के लिए, अभी ऑनलाइन हों!",
  LIVE_DASHBOARD : "लाइव आँकड़े डैशबोर्ड",
  CLICK_TO_ACCESS_YOUR_ACCOUNT : "अपने खाते तक पहुंचने के लिए यहां क्लिक करें",
  ADD_ALTERNATE_NUMBER : "अपना दूसरा मोबाइल नंबर ऐड करे",
  ENTER_ALTERNATE_MOBILE_NUMBER : "अपना दूसरा मोबाइल नंबर एंटर करे",
  PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER : "कृपया 10 नंबर का सही नंबर एंटर करे",
  ALTERNATE_MOBILE_NUMBER : "दूसरा मोबाइल नंबर",
  REMOVE : "निकाले",
  REMOVE_ALTERNATE_NUMBER : "दूसरा मोबाइल नंबर हटाएं",
  ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER : "क्या आप अपना दूसरा मोबाइल नंबर निकालना चाहते हैं?",
  YES_REMOVE_IT : "हाँ, इसे निकाल दें",
  NUMBER_REMOVED_SUCCESSFULLY : "मोबाइल नंबर सफलतापूर्वक निकाल दिया गया है",
  EDIT_ALTERNATE_MOBILE_NUMBER : "दूसरे मोबाइल नंबर में बदलाव करे",
  NUMBER_ADDED_SUCCESSFULLY : "मोबाइल नंबर एडेड",
  NUMBER_EDITED_SUCCESSFULLY : "मोबाइल नंबर अपडेटेड",
  ALTERNATE_MOBILE_OTP_LIMIT_EXCEED : "ओटीपी सीमा पार हो गई, नंबर और ओटीपी फिर से दर्ज करें",
  WRONG_OTP : "कृपया सही ओटीपी एंटर करें",
  ATTEMPTS_LEFT : "3 और प्रयास बाकी हैं",
  ATTEMPT_LEFT : "प्रयास बाकी है" ,
  OTP_LIMIT_EXCEEDED : "आपकी ओटीपी की सीमा पार हो गई है",
  OTP_LIMIT_EXCEEDED_MESSAGE :"आपने अपनी ओटीपी की सीमा पार कर ली हैं। कृपया 10 मिनट बाद पुनः प्रयास करें।",
  TRY_AGAIN_LATER : "बाद में प्रयास करे",
  NUMBER_ALREADY_EXIST_ERROR : "यह नंबर किसी और अकाउंट में इस्तेमाल में है ! कृपया दूसरा नंबर ऐड करे",
  OTP_RESEND_LIMIT_EXCEEDED : "ओटीपी पुनः भेजने की सीमा पार हो गई है",
  LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER : "कृपया कुछ समय बाद प्रयास करे" ,
  ALTERNATE_NUMBER_CANNOT_BE_ADDED : "दूसरा मोबाइल नंबर ऐड नहीं हो सकता",
  VERIFICATION_IS_TAKING_A_BIT_LONGER : "ऐसा लगता है कि आपका सत्यापन अपेक्षा से थोड़ा अधिक समय ले रहा है।\nआप सहायता के लिए सहायता से संपर्क कर सकते हैं।",
  COMPLETE_ONBOARDING : "पूर्ण ऑनबोर्डिंग",
  ADD_ALTERNATE_NUMBER_IN_MEANTIME :  "इस प्रोसेस को पूरा होने में 2 कामकाजी दिन \n तक लग सकते हैं। इस बीच, आप \nएक वैकल्पिक मोबाइल नंबर जोड़ सकते हैं।",
  PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS : "इस मोबाइल नंबर वाला व्यक्ति पहले से मौजूद है।",
  OTP_ : "OTP",
  MAPS: "Maps",
  DEMO : "DEMO",
  PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP : "कृपया ग्राहक से ओटीपी के लिए पूछें",
  DELETE : "मिटाना",
  VIEW : "देखना",
  ISSUE_NO : "निर्गत संख्या",
  ADD_VOICE_NOTE : "वॉइस नोट जोड़ें",
  VOICE_NOTE_ADDED : "वॉयस नोट जोड़ा गया",
  SUBMIT_ISSUE_DETAILS : "समस्या विवरण प्रस्तुत करें",
  IMAGE_PREVIEW : "छवि पूर्वावलोकन",
  RIDE_REPORT_ISSUE : "समस्या की रिपोर्ट करने के लिए राइड चुनें",
  ADDED_IMAGES : "जोड़े गए चित्र",
  NO_IMAGES_ADDED : "कोई चित्र नहीं जोड़ा गया",
  ASK_DETAILS_MESSAGE : "कृपया कुछ और विवरण दें। आप बेहतर ढंग से विस्तृत करने के लिए चित्र या वॉइस नोट भी भेज सकते हैं।",
  ASK_DETAILS_MESSAGE_REVERSED : "कृपया खोई हुई वस्तु के बारे में अधिक जानकारी साझा करें। आप बेहतर ढंग से विस्तृत करने के लिए चित्र या वॉइस नोट भी भेज सकते हैं।",
  SELECT_OPTION : "कृपया हमें बताएं कि क्या आप इनमें से किसी का सामना कर रहे हैं",
  SELECT_OPTION_REVERSED : "आप इस मुद्दे को कैसे सुलझाना चाहते हैं?",
  ISSUE_SUBMITTED_MESSAGE : "विवरण प्राप्त हुआ! आपकी समस्या का समाधान करने के लिए हमारी टीम 24 घंटे के भीतर आपको कॉल करेगी।",
  I_DONT_KNOW_WHICH_RIDE : "मुझे नहीं पता कि कौन सी सवारी",
  REPORT_ISSUE_CHAT_PLACEHOLDER : "अपने मामले का वर्णन करें। नम्मा यात्री 24 घंटे के भीतर इसे हल करने का प्रयास करेंगे।",
  ADDED_VOICE_NOTE : "जोड़ा आवाज नोट",
  NO_VOICE_NOTE_ADDED : "कोई वॉयस नोट नहीं जोड़ा गया",
  CALL_CUSTOMER_TITLE : "ग्राहक को कॉल करें?",
  CALL_CUSTOMER_DESCRIPTION : "आप ग्राहक को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
  PLACE_CALL : "कॉल करें",
  ADD_IMAGE : "छवि जोड़ें",
  ADD_ANOTHER : "एक और जोड़ें",
  IMAGES_ADDED : "छवियों को जोड़ा गया",
  ISSUE_SUBMITTED_TEXT : "ज़रा ठहरो! हम आपकी समस्या को हल करने पर काम कर रहे हैं",
  CHOOSE_AN_OPTION : "जारी रखने के लिए एक विकल्प चुनें",
  IMAGE_ADDED : "छवि जोड़ी गई",
  DONE : "पूर्ण",
  RECORD_VOICE_NOTE : "रिकॉर्ड आवाज नोट",
  HELP_AND_SUPPORT : "मदद समर्थन",
  MORE_OPTIONS : "अधिक विकल्प",
  ONGOING_ISSUES : "चल रहे मुद्दे",
  RESOLVED_ISSUES : "सुलझाए गए मुद्दे",
  RESOLVED_ISSUE : "हल किया गया मुद्दा",
  ONGOING_ISSUE : "चल रहे मुद्दे",
  LOST_ITEM : "खोई हुई वस्तु",
  RIDE_RELATED_ISSUE : "सवारी संबंधी समस्या",
  APP_RELATED_ISSUE : "ऐप संबंधित समस्या",
  FARE_RELATED_ISSUE : "किराया संबंधित मुद्दा",
  ISSUE_NUMBER : "निर्गत संख्या  ",
  REMOVE_ISSUE : "मुद्दा हटाओ",
  CALL_SUPPORT_NUMBER : "समर्थन से संपर्क करें",
  YEARS_AGO : " साल पहले",
  MONTHS_AGO : " महीने पहले",
  DAYS_AGO : " दिन पहले",
  HOURS_AGO : " घंटे पहले",
  MIN_AGO : " मिनट पहले",
  SEC_AGO : " सेकंड पहले",
  ISSUE_REMOVED : "मुद्दा हटाया गया",
  LOADING :"लोड हो रहा है",
  MAX_IMAGES : "अधिकतम 3 चित्र अपलोड किए जा सकते हैं",
  APP_RELATED : "ऐप संबंधित",
  FARE_RELATED : "किराया संबंधित",
  RIDE_RELATED : "सवारी संबंधित",
  LOST_AND_FOUND : "लॉस्ट एंड फाउंड ",
  REPORT_LOST_ITEM : "खोई हुई वस्तु की सूचना दें",
  MAKE_YOURSELF_AVAILABLE_FOR : "निम्नलिखित के लिए स्वयं को उपलब्ध कराएं",
  SELECT_YOUR_GENDER: "अपना लिंग चुनें",
  FEMALE: "महिला",
  MALE: "नर",
  PREFER_NOT_TO_SAY: "चुप रहना पसंद करूंगा",
  GENDER : "लिंग",
  SET_NOW : "अभी सेट करें",
  COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES : "अधिक राइड पाने के लिए अपनी प्रोफ़ाइल पूरी करें!",
  UPDATE_NOW : "अभी अपडेट करे",
  CONFIRM : "पुष्टि करें",
  GENDER_UPDATED : "लिंग अद्यतन",
  CORPORATE_ADDRESS : "कॉर्पोरेट पता",
  CORPORATE_ADDRESS_DESCRIPTION : "जसपे टेक्नोलॉजीज प्राइवेट लिमिटेड <br> गिरिजा बिल्डिंग, नंबर 817, गणपति मंदिर रोड, 8वां ब्लॉक, कोरमंगला, बेंगलुरु, कर्नाटक 560095, भारत।",
  CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL : "वेबसाइट : <u>https://nammayatri.in/</u>",
  REGISTERED_ADDRESS : "पंजीकृत पता",
  REGISTERED_ADDRESS_DESCRIPTION : "जस्पे टेक्नोलॉजीज प्राइवेट लिमिटेड <br> स्टैलियन बिजनेस सेंटर, नंबर 444, तीसरी और चौथी मंजिल, 18वीं मेन, 6वां ब्लॉक, कोरमंगला बेंगलुरु, कर्नाटक- 560095, भारत।",
  REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL : "वेबसाइट : <u>https://nammayatri.in/</u>",
  ZONE_CANCEL_TEXT_DROP : "आपका ग्राहक शायद समय पर मेट्रो स्टेशन पहुंचने की हड़बड़ी में है! \n हम आपसे अनुरोध करते हैं कि रद्द न करें।",
  ZONE_CANCEL_TEXT_PICKUP : "आपका ग्राहक शायद अपने गंतव्य तक पहुंचने की हड़बड़ी में है। \n हम आपसे अनुरोध करते हैं कि रद्द न करें।"

}