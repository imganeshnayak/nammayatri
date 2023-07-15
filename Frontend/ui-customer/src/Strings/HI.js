export function getStringValue(key) {
  if (key in hindiStrings) {
    return hindiStrings[key];
  }
  console.error(key + " not found in hindiString");
  return "";
}

const hindiStrings = {
  "DOWNLOAD_INVOICE": "इनवाइस को डाउनलोड करें",
  "REPORT_AN_ISSUE": "मामले की रिपोर्ट दर्ज करें",
  "SUBMIT": "जमा करें",
  "VIEW_INVOICE": "बिल देखें",
  "YOU_RATED": "आपकी रेटिंग",
  "TOTAL_AMOUNT": "कुल राशि",
  "AMOUNT_PAID": "राशि का भुगतान",
  "TRIP_DETAILS_": "यात्रा विवरण",
  "DOWNLOAD_PDF": "डाउनलोड पीडीऍफ़",
  "CGST": "सीजीएसटी",
  "INVOICE": "बिल",
  "TRIP_CHARGES": "यात्रा शुल्क",
  "PROMOTION": "पदोन्नति",
  "SEND_EMAIL": "ईमेल भेजें",
  "YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE": "आप यहां इस मुद्दे का वर्णन कर सकते हैं",
  "THANK_YOU_FOR_WRITING": "हमें सूचित करने के लिए धन्यवाद!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE": "हमें आपका मुद्दा मिल गया है। हम कुछ समय में आप तक पहुंचेंगे.",
  "GO_HOME_": "गो होम",
  "LOGO": "प्रतीक चिन्ह",
  "ABOUT_APP_DESCRIPTION": "नम्मा यात्री ड्राइवरों के साथ सवारों को जोड़ने के लिए एक खुला मंच है। ऐप सवारों के लिए मीटर दर के साथ एक सवारी बुक करने के लिए सुविधाजनक बनाता है इसलिए न्यूनतम किराया",
  "TERMS_AND_CONDITIONS": "टी & सी",
  "ABOUT": "एप के बारे में",
  "PRIVACY_POLICY": "गोपनीयता नीति",
  "SET_UP_YOUR_ACCOUNT": "अपना खाता स्थापित करें",
  "CONTINUE": "जारी रखें",
  "ENTER_YOUR_NAME": "अपना नाम दर्ज करें",
  "  FULL_NAME": "पूरा नाम",
  "EMAIL": "ईमेल",
  "WELCOME_TEXT": "नम्मा यात्री में आपका स्वागत है",
  "PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE": "जारी रखने के लिए कृपया अपनी पसंदीदा भाषा चुनें।",
  "WRITE_TO_US": "हमे संपर्क करे",
  "NOTE": "टिप्पणी: ",
  "VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS": "विशिष्ट शिकायतों के लिए मेरी सवारी विकल्प पर जाएँ",
  "THANK_YOU_FOR_WRITING_TO_US": "हमें सूचित करने के लिए धन्यवाद!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME": "हमें आपका मुद्दा मिला है। हम कुछ समय में आपके पास पहुंचेंगे।",
  "GO_TO_HOME__": "होम",
  "SUBJECT": "विषय",
  "YOUR_EMAIL_ID": "आपकी ईमेल आईडी",
  "DESCRIBE_YOUR_ISSUE": "अपनी समस्या का वर्णन करें",
  "ENTER_MOBILE_NUMBER": "मोबाइल नंबर दर्ज करें",
  "BY_TAPPING_CONTINUE": "जारी रखें पर क्लिक करके, आप हमारी सहमति देते हैं",
  "  TO_THE": "आप सहमत हैं कि आप स्वीकार कर रहे हैं",
  "ENTER_OTP": "OTP दर्ज करें",
  "RESEND": "पुन: भेजें",
  "ENTER_YOUR_MOBILE_NUMBER": "अपना मोबाइल नंबर दर्ज करें",
  "LOGIN_USING_THE_OTP_SENT_TO": "भेजे गए OTP का उपयोग करके लॉगिन करें",
  "YOUR_RECENT_RIDE": "आपकी हालिया सवारी",
  "VIEW_ALL_RIDES": "सभी सवारी देखें",
  "ALL_TOPICS": "सभी विषय",
  "FAQ": "सामान्य प्रश्न",
  "REPORT_AN_ISSUE_WITH_THIS_TRIP": "इस यात्रा के साथ एक मुद्दे की रिपोर्ट करें",
  "GETTING_STARTED_AND_FAQS": "आरंभ करना और अक्सर पूछे जाने वाले प्रश्न",
  "FOR_OTHER_ISSUES_WRITE_TO_US": "अन्य मुद्दों के लिए, हमें लिखें",
  "HELP_AND_SUPPORT": "सहायता और समर्थन",
  "OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS": "इस यात्रा के लिए हमारी सुझाई गई कीमत है",
  "DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE": "*ड्राइवर उपरोक्त सीमा के बीच चार्ज कर सकते हैं",
  "HOW_THIS_WORKS": "यह कैसे काम करता है?",
  "FINDING_RIDES_NEAR_YOU": "आप के पास सवारी ढूंढना ...",
  "CONFIRMING_THE_RIDE_FOR_YOU": "आप के लिए सवारी की पुष्टि ...",
  "CANCEL_SEARCH": "खोज रद्द करें",
  "YOUR_RIDE_IS_NOW_COMPLETE": "आपकी सवारी अब पूरी हो गई है!",
  "PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH": "कृपया ड्राइवर को सीधे अंतिम राशि का भुगतान करें",
  "WHERE_TO": "कहाँ ?",
  "HOME": "घर",
  "PICK_UP_LOCATION": "पिकअप करने की जगह",
  "REQUEST_RIDE": "अनुरोध सवारी",
  "NAME": "नाम",
  "MOBILE_NUMBER_STR": "मोबाइल नंबर",
  "PERSONAL_DETAILS": "व्यक्तिगत विवरण",
  "YOUR_RIDES": "आपकी सवारी",
  "YOU_ARE_OFFLINE": "आप ऑफ़लाइन हो",
  "CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN": "अपने इंटरनेट कनेक्शन की जाँच करें और पुनः प्रयास करें",
  "TRY_AGAIN": "पुनः प्रयास करें",
  "THANK_YOUR_DRIVER": "🙏 धन्यवाद ड्राइवर!",
  "HOPE_YOUR_RIDE_WAS_HASSLE_FREE": "आशा है कि आपकी सवारी परेशानी मुक्त थी",
  "HOW_WAS_YOUR_RIDE_WITH": " के साथ आपकी सवारी कैसी थी?",
  "GOT_IT_TELL_US_MORE": " समझ गये, हमे और सुचना दे",
  "WRITE_A_COMMENT": "एक टिप्पणी लिखें (वैकल्पिक)",
  "UPDATE": "अपडेट",
  "LANGUAGE": "भाषा",
  "OTP": "ओटीपी",
  "PAYMENT_METHOD": "भुगतान का तरीका",
  "PAYMENT_METHOD_STRING": "कैश / यूपीआई ऐप का इस्तेमाल करें",
  "PAYMENT_METHOD_STRING_": "कैश / यूपीआई ऐप का इस्तेमाल करें",
  "CANCEL_RIDE": "मेरी राइड रद्द करें",
  "SUPPORT": "सहायता",
  "PICKUP_AND_DROP": "पिकअप और ड्रॉप",
  "CANCELLED": "रद्द",
  "HOW_THE_PRICING_WORKS": "मूल्य निर्धारण कैसे काम करता है?",
  "SELECT_AN_OFFER": "एक प्रस्ताव का चयन करें",
  "CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT": "अपने आराम के अनुसार एक सवारी चुनें",
  "IT_SEEMS_TO_BE_A_VERY_BUSY_DAY": "यह बहुत व्यस्त दिन लगता है। आप फिर से सवारी की तलाश कर सकते हैं",
  "SORT_BY": "इसके अनुसार क्रमबद्ध करें",
  "SORRY_WE_COULDNT_FIND_ANY_RIDES": "क्षमा करें, हमें कोई सवारी नहीं मिली",
  "LOAD_MORE": "और लोड करें",
  "WE_NEED_ACCESS_TO_YOUR_LOCATION": "हमें आपके स्थान तक पहुंच की आवश्यकता है!",
  "LOCATION_PERMISSION_SUBTITLE": "आपको राइड दिलाने के लिए, हमें आपके डिवाइस की लोकेशन की आवश्यकता होती है।",
  "CALL": "कॉल",
  "EMPTY_RIDES": "खाली सवारी सूची",
  "YOU_HAVENT_TAKEN_A_TRIP_YET": "आपने अभी तक कोई यात्रा नहीं की है!",
  "BOOK_NOW": "अभी बुक करें",
  "T_AND_C_A": "ए) आप इस बात से सहमत हैं कि आप बीटा परीक्षण के इच्छुक प्रतिभागी हैं और जुसपे का किसी भी संबंध में आपके खिलाफ कोई दायित्व नहीं होगा",
  "DATA_COLLECTION_AUTHORITY": "सी) मैं अपनी जानकारी एकत्र करने के लिए Juspay को नियुक्त और अधिकृत करता हूं, और जारी रखने से, मैं उपयोग की शर्तों और गोपनीयता नीति से सहमत हूं।",
  "DENY_ACCESS": "अस्वीकार करें",
  "PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL": "कृपया हमें बताएं कि आप रद्द क्यों करना चाहते हैं",
  "MANDATORY": "अनिवार्य",
  "SOFTWARE_LICENSE": "सॉफ़्टवेयर लाइसेंस",
  "LOGOUT_": "लॉग आउट",
  "REQUEST_AUTO_RIDE": "ऑटो राइड बुक करें",
  "RATE_YOUR_RIDE": "अपनी सवारी का मूल्यांकन करें",
  "SKIP": "स्किप करें",
  "ERROR_404": "त्रुटि 404",
  "PROBLEM_AT_OUR_END": "हमारे अंत में एक समस्या प्रतीत होती है। जब हम फिर से उपलब्ध हों तो सूचित करें",
  "NOTIFY_ME": "मुझे सूचित करो",
  "ADDRESS": "पता",
  "CHANGE": "परिवर्तन",
  "SAVE_AS": "इस प्रप में सहेजें",
  "ADD_TAG": "टैग जोड़ो",
  "WORK": "काम",
  "OTHER": "अन्य",
  "SAVE": "सहेजें",
  "ADD_NEW_ADDRESS": "नया पता जोड़ें",
  "SAVED_ADDRESSES": "सहेजा गया पता",
  "ADDRESSES": "पतों",
  "NO_FAVOURITES_SAVED_YET": "अभी तक कोई पसंदीदा सहेजा नहीं गया है",
  "SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY": "पसंदीदा स्थान आपके अक्सर देखे जाने वाले स्थानों को आसान रखने में मदद करता है",
  "EMERGENCY_CONTACTS": "आपातकालीन संपर्क",
  "ADD_EMERGENCY_CONTACTS": "आपातकालीन संपर्क जोड़ें",
  "NO_EMERGENCY_CONTACTS_SET": "कोई आपातकालीन संपर्क सेट नहीं",
  "EMERGENCY_CONTACTS_SCREEN_DESCRIPTION": "आप आपातकालीन स्थिति में 3 आपातकालीन संपर्कों के साथ अपनी सवारी की स्थिति साझा कर सकते हैं",
  "COPIED": "कॉपी किया गया",
  "TRIP_ID": "ट्रिप आईडी",
  "SAVE_PLACE": "स्थान सहेजें",
  "RIDE_FARE": "यात्रा राशि",
  "ASK_FOR_PRICE": "कीमत पूछे",
  "ASK_FOR_PRICE_INFO": "आपको ड्राइवरों द्वारा तय की गई पिक-अप दूरी के लिए अतिरिक्त 10 रुपये के मामूली शुल्क के साथ सरकार द्वारा निर्धारित आधार मूल्य के आधार पर किराया मिलेगा। कुछ ड्राइवर अतिरिक्त कारकों जैसे ट्रैफ़िक, वापसी यात्रा की संभावना आदि को कवर करने के लिए केवल अपने विवेक पर नाममात्र बख्शीश का अनुरोध कर सकते हैं।",
  "GET_ESTIMATE_FARE": "अनुमानित किराया प्राप्त करें",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS": "एक प्रस्ताव का चयन करें (वैकल्पिक)",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO": "डिफ़ॉल्ट रूप से, जब  “राइड को ऑटो-असाइन करें” चयनित होता है, तो आपको एक ड्राइवर असाइन किया जाएगा जो अनुमान सीमा के भीतर पहले स्वीकार करता है। इसके बजाय, यदि आप ड्राइवर ऑफ़र चुनना चाहते हैं, तो आप इस सुविधा का उपयोग करने के लिए अचयनित कर सकते हैं।",
  "PAY_THE_DRIVER": "ड्राइवर को भुगतान करें",
  "PAY_THE_DRIVER_INFO": "ड्राइवर को उस कीमत का भुगतान करें जिसकी आपने पुष्टि की थी।. ",
  "PAY_THE_DRIVER_NOTE": "(सवारी की दूरी बदलने पर कुल किराया बदल सकता है)",
  "UPDATE_PERSONAL_DETAILS": "व्यक्तिगत विवरण अपडेट करें",
  "EDIT": "संपादन करना",
  "DEL_ACCOUNT": "खाता हटा दो",
  "ACCOUNT_DELETION_CONFIRMATION": "क्या आप इस खाते को हटाने के लिए सुनिश्चित हैं? आपके सभी व्यक्तिगत डेटा खो जाएंगे",
  "REQUEST_SUBMITTED": "अनुरोध सबमिट किया गया",
  "WE_WILL_DELETE_YOUR_ACCOUNT": "हमें यह देखकर खेद है कि आपने हमारे मंच को छोड़ दिया। आपका खाता अगले 30 दिनों के भीतर हटा दिया जाएगा। इस बीच यदि आप अपना खाता बनाए रखना चाहते हैं, तो कृपया हमारे ग्राहक सहायता नंबर पर कॉल करें",
  "YES_DELETE_IT": "हाँ, इसे हटा दें",
  "REQUEST_TO_DELETE_ACCOUNT": "खाता हटाने का अनुरोध करें",
  "CANCEL_STR": "रद्द करें",
  "LOADING": "लोड हो रहा है",
  "PLEASE_WAIT_WHILE_IN_PROGRESS": "कृपया प्रगति के दौरान प्रतीक्षा करें",
  "SET_LOCATION_ON_MAP": "मानचित्र पर स्थान निर्धारित करें",
  "CURRENT_LOCATION": "वर्तमान स्थान",
  "ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN": "वास्तविक किराया जो दिखाया गया था उससे अधिक था।",
  "DELETE": "मिटायें",
  "ARE_YOU_SURE_YOU_WANT_TO_LOGOUT": "क्या आप लॉग आउट करना चाहते हैं ?",
  "ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "क्या आप रद्द करना चाहते हैं ?",
  "YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "आपके पास राइड ऑफ़र हैं, क्या आप वाकई रद्द करना चाहते हैं?",
  "GO_BACK_": "वापस जाओ",
  "REGISTER_USING_DIFFERENT_NUMBER": "क्या आप किसी भिन्न मोबाइल नंबर का उपयोग करके पंजीकरण करना चाहते हैं?",
  "YES": "हाँ",
  "NO": "नहीं",
  "CANCEL_": "रद्द करें",
  "IS_ON_THE_WAY": "रास्ते मे है..",
  "ENTER_4_DIGIT_OTP": "4 अंकों का ओटीपी दर्ज करें",
  "WRONG_OTP": "गलत ओटीपी",
  "GRANT_ACCESS": "पहुँच प्रदान करें",
  "ENTER_A_LOCATION": "एक स्थान दर्ज करें",
  "NEARBY": "नज़दीक",
  "MINS_AWAY": "मिनट दूर",
  "PAID": "प्रदत्त",
  "BY_CASH": "नकद द्वारा",
  "ONLINE_": "ऑनलाइन",
  "USER": "उपयोगकर्ता",
  "EMAIL_ALREADY_EXISTS": "ईमेल अपडेट करने में विफल. ईमेल पहले से मौजूद है।",
  "IN": "में",
  "VERIFYING_OTP": "सत्यापन ओटीपी",
  "TRACK_LIVE_LOCATION_USING": "लाइव स्थान का उपयोग करके ट्रैक करें",
  "GOOGLE_MAP_": "गूगल मेप",
  "IN_APP_TRACKING": "ऐप ट्रैकिंग में",
  "REQUEST_TIMED_OUT": "अनुरोध का समय समाप्त",
  "LIMIT_EXCEEDED": "सीमा पार हो गई",
  "ERROR_OCCURED": "त्रुटि हुई",
  "QUOTE_EXPIRED": "भाव समाप्त",
  "GETTING_ESTIMATES_FOR_YOU": "आपके लिए अनुमान प्राप्त किए जा रहे हैं...",
  "CONFIRM_PICKUP_LOCATION": "पिकअप स्थान की पुष्टि करें",
  "CONFIRM_DROP_LOCATION": "ड्रॉप स्थान की पुष्टि करें",
  "NO_DRIVERS_AVAILABLE": "कोई ड्राइवर उपलब्ध नहीं है",
  "ERROR_OCCURED_TRY_AGAIN": "त्रुटि हुई। पुनः प्रयास करें",
  "ERROR_OCCURED_TRY_AFTER_SOMETIME": "त्रुटि हुई। कृपया पुन: प्रयास करें",
  "ASKED_FOR_MORE_MONEY": "और पैसे मांगे",
  "START_": "प्रारंभ",
  "LIMIT_REACHED": "सीमा पार हो गई। कुछ देर बाद कोशिश करें",
  "RIDE_NOT_SERVICEABLE": "सवारी सेवा योग्य नहीं",
  "CONFIRM_FOR": "के लिए पुष्टि करें",
  "ETA_WAS_TOO_SHORT": "ईटीए बहुत छोटा था।",
  "DRIVER_REQUESTED_TO_CANCEL": "ड्राइवर ने मुझे रद्द करने का अनुरोध किया",
  "PICK_UP_LOCATION_INCORRECT": "पिकअप का स्थान गलत था।",
  "COULD_NOT_CONNECT_TO_DRIVER": "मैं ड्राइवर से कनेक्ट नहीं हो सका।",
  "ETA_WAS_TOO_LONG": "ईटीए बहुत लंबा था।",
  "OTHERS": "अन्य",
  "DESTINATION_OUTSIDE_LIMITS": "दर्ज किया गया गंतव्य शहर की सीमा के बाहर है",
  "DROP_LOCATION_FAR_AWAY": "आपका ड्रॉप स्थान बहुत दूर है",
  "CHANGE_DROP_LOCATION": "ड्रॉप स्थान बदलें",
  "YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING": "आप पैदल चलकर जा सकते हैं या सवारी बुकिंग जारी रख सकते हैं",
  "YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST": "आपकी यात्रा बहुत छोटी है। आप अपने गंतव्य से सिर्फ ",
  "METERS_AWAY_FROM_YOUR_DESTINATION": " मीटर दूर हैं!",
  "BOOK_RIDE_": "सवारी बुक करें",
  "LOCATION_UNSERVICEABLE": "स्थान अनुपयोगी",
  "CURRENTLY_WE_ARE_LIVE_IN_": "वर्तमान में हम बेंगलुरु और मैसूरु में लाइव हैं, आप वहां हमारी सेवाओं का आनंद ले सकते हैं",
  "CHANGE_LOCATION": "स्थान बदलें",
  "IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE": "यदि आप अभी भी सवारी बुक करना चाहते हैं तो जारी रखें पर क्लिक करें और सवारी की बुकिंग शुरू करें",
  "THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE": "यात्रा बहुत छोटी है और इसे पूरा करने के लिए केवल ",
  "STEPS_TO_COMPLETE": " चरणों की आवश्यकता है",
  "CANCEL_AUTO_ASSIGNING": "ऑटो असाइनिंग रद्द करें",
  "AUTO_ACCEPTING_SELECTED_RIDE": " में ऑटो स्वीकार",
  "HELP_US_WITH_YOUR_REASON": "अपने कारण से हमारी मदद करें",
  "MAX_CHAR_LIMIT_REACHED": "अधिकतम वर्ण सीमा पूरी हुई,",
  "DRIVER_WAS_NOT_REACHABLE": "ड्राइवर उपलब्ध नहीं था",
  "SHOW_ALL_OPTIONS": "सभी विकल्प दिखाएं",
  "EXPIRES_IN": "में समाप्त",
  "PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI": "*Cash/UPI का उपयोग करके सीधे अपने ड्राइवर को भुगतान करें",
  "UPDATE_REQUIRED": "अद्यतन आवश्यक है",
  "PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE": "कृपया सेवा जारी रखने के लिए ऐप को अपडेट करें",
  "NOT_NOW": "अभी नहीं",
  "OF": "में से",
  "LOST_SOMETHING": "क्या आपने कुछ खोया है?",
  "TRY_CONNECTING_WITH_THE_DRIVER": "आप सीधे ड्राइवर पार्टनर से जुड़ने के लिए कॉलबैक का अनुरोध कर सकते हैं",
  "CALL_DRIVER": "ड्राइवर से संपर्क करें",
  "NO_MORE_RIDES": "कोई और सवारी नहीं",
  "CONTACT_SUPPORT": "सहयोग टीम से संपर्क करें",
  "INVALID_MOBILE_NUMBER": "गलत मोबाइल नंबर",
  "CONFIRM_LOCATION": "स्थान की पुष्टि करें",
  "RIDE_COMPLETED": "सवारी पूरी हुई",
  "SUBMIT_FEEDBACK": "प्रतिपुष्टि दें",
  "HOW_WAS_YOUR_RIDE_EXPERIENCE": "आपकी सवारी का अनुभव कैसा रहा?",
  "DROP": "ड्रॉप",
  "RATE_YOUR_RIDE_WITH": " के साथ अपनी राइड को रेट करें",
  "VIEW_BREAKDOWN": "ब्रेकडाउन देखें",
  "PAY_DRIVER_USING_CASH_OR_UPI": "Cash/UPI का उपयोग कर ड्राइवर को भुगतान करें",
  "PAY_DRIVER_USING_CASH_OR_UPI_": "Cash/UPI का उपयोग कर ड्राइवर को भुगतान करें",
  "RATE_YOUR_DRIVER": "अपने ड्राइवर को रेट करें",
  "MY_RIDES": "मेरी सवारी",
  "RIDE_ID": "सवारी आईडी",
  "RIDE_DETAILS": "सवारी विवरण",
  "SELECT_A_RIDE": "ज़्यादा जानकारी के लिए कोई राइड चुनें",
  "CONFIRM_RIDE_": "सवारी की पुष्टि करें",
  "YOU_CAN_CANCEL_RIDE": "ड्राइवर पार्टनर से ऑफ़र मिलने के बाद आप रद्द कर सकते हैं",
  "ESTIMATES_CHANGED": "आपकी राइड की अनुमानित कीमत अब बदल गई है ",
  "ESTIMATES_REVISED_TO": "संशोधित अनुमानित कीमत है",
  "RATE_CARD": "रेट कार्ड",
  "NIGHT_TIME_CHARGES": "रात के समय का शुल्क",
  "MIN_FARE_UPTO": "न्यूनतम किराया 2 किमी तक",
  "RATE_ABOVE_MIN_FARE": "न्यूनतम किराए से ऊपर की दर",
  "DRIVER_PICKUP_CHARGES": "ड्राइवर के आने का शुल्क",
  "DAY_TIMES_OF": "रात में लागू दिन के समय के शुल्क का ",
  "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT": " गुना शुल्क से रात 10 बजे से सुबह 5 बजे तक",
  "NIGHT_TIMES_OF": "रात (🌙) 10 बजे से सुबह 5 बजे तक के किराए पर दिन के समय का ",
  "DAYTIME_CHARGES_APPLIED_AT_NIGHT": " गुना शुल्क लगता है",
  "DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "* चालक वैकल्पिक रूप से यातायात, वापसी यात्रा की संभावना आदि को कवर करने के लिए आधार किराए के 10% (निकटतम रु.10 तक) का अनुरोध कर सकता है।",
  "GOT_IT": "समझ गया!",
  "DAY_TIME_CHARGES": "दिन के समय शुल्क",
  "SHARE_APP": "ऐप शेयर करें",
  "AWAY_C": "दूर",
  "AWAY": "दूर",
  "AT_PICKUP": "At Pickup",
  "FARE_UPDATED": "किराया अपडेट किया",
  "TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE": "रूट में बदलाव के कारण कुल किराए में बदलाव हो सकता है",
  "AT_DROP": "At Drop",
  "EMERGENCY_HELP": "आपातकालीन सहायता",
  "CALL_POLICE": "पुलिस को कॉल करें ",
  "ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION": "आपकी सवारी की स्थिति और स्थान भी साझा करे",
  "SHARE_RIDE_WITH_EMERGENCY_CONTACTS": "आपातकालीन संपर्कों के साथ राइड शेयर करें",
  "DO_YOU_NEED_EMERGENCY_HELP": "क्या आपको आपातकालीन सहायता की आवश्यकता है ?",
  "CALL_SUPPORT": "कॉल सपोर्ट",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप नम्मा यात्री सपोर्ट टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
  "YOU_ARE_ABOUT_TO_CALL_NEAREST_EMERGENCY_CENTRE": "आप निकटतम आपातकालीन केंद्र पर कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
  "DIAL_112": "डायल 112",
  "HELP_US_WITH_YOUR_FEEDBACK": "अपने फीडबैक से हमारी सहायता करें",
  "WAIT_TIME": "प्रतीक्षा समय",
  "FAVOURITES": "पसंदीदा",
  "ADD_FAVOURITE": "पसंदीदा जोड़ें",
  "ALL_FAVOURITES": "सभी पसंदीदा",
  "REMOVE": "हटाएं",
  "SELECT_ON_MAP": "मानचित्र पर चयन करें",
  "FAVOURITE_LOCATION": "पसंदीदा स्थान",
  "EDIT_FAVOURITE": "पसंदीदा संपादित करें",
  "DRAG_THE_MAP": "मानचित्र को खींचें और पिन को सटीक स्थान पर सेट करें",
  "CHOOSE_ON_MAP": "मानचित्र पर चुनें",
  "USE_CURRENT_LOCATION": "वर्तमान स्थान का प्रयोग करें",
  "FAVOURITE_YOUR_CURRENT_LOCATION": "अपने वर्तमान स्थान को पसंदीदा बनाएं",
  "LOCATION": "जगह",
  "LOCATION_ALREADY_EXISTS_AS": "पहले से ही मौजूद है",
  "GIVE_THIS_LOCATION_A_NAME": "इस स्थान को एक नाम दें",
  "FAVOURITE": "पसंदीदा",
  "CONFIRM_AND_SAVE": "कन्फर्म & सेव",
  "REMOVE_FAVOURITE": "पसंदीदा हटाएं",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_": "क्या आप वाकई पसंदीदा को हटाना \n चाहते हैं?",
  "YES_REMOVE": "हाँ, हटाओ",
  "ADD_NEW_FAVOURITE": "नया पसंदीदा जोड़ें",
  "SELECT_YOUR_DROP": "अपना ड्रॉप चुनें",
  "FAVOURITE_REMOVED_SUCCESSFULLY": "पसंदीदा सफलतापूर्वक निकाला गया",
  "LOCATION_ALREADY_EXISTS": "लोकेशन पहले से मौजूद है",
  "FAVOURITE_LIMIT_REACHED": "पसंदीदा सीमा पार हो गई",
  "LOCATION_ALREADY": "स्थान पहले से ही",
  "EXISTS_AS": "के रूप में मौजूद है",
  "FAVOURITE_ADDED_SUCCESSFULLY": "पसंदीदा सफलतापूर्वक जोड़ा गया",
  "FAVOURITE_UPDATED_SUCCESSFULLY": "पसंदीदा सफलतापूर्वक अपडेट किया गया",
  "ALREADY_EXISTS": "पहले से ही मौजूद है",
  "NAME_ALREADY_IN_USE": "नाम पहले से उपयोग में है",
  "SELECT_FAVOURITE": "पसंदीदा चुनें",
  "CONFIRM_CHANGES": "परिवर्तनों की पुष्टि करें",
  "ADD_SAVED_LOCATION_FROM_SETTINGS": "*आप साइड मेनू > पसंदीदा से नया पसंदीदा जोड़ सकते हैं",
  "YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS": "आपको अधिकतम 3 संपर्कों का चयन करने के लिए कहा जाएगा",
  "AUTO_ASSIGN_A_RIDE": "राइड को ऑटो-असाइन करें",
  "IS_WAITING_FOR_YOU": "इंतज़ार कर रहे है...",
  "WAIT_TIME_TOO_LONG": "प्रतीक्षा समय बहुत लंबा है",
  "GOT_ANOTHER_RIDE_ELSE_WHERE": "दूसरी सवारी मिली",
  "DRIVER_WAS_RUDE": "ड्राइवर बदतमीजी कर रहा था",
  "MAYBE_LATER": "शायद बाद में",
  "YOUR_RIDE_HAS_STARTED": "वाह! आपकी राइड शुरू हो गई है 🤩",
  "ENJOY_RIDING_WITH_US": "हमारे साथ सवारी का आनंद ले रहे हैं? शब्द \n और खुशियाँ शेयर करें",
  "VIEW_DETAILS": "विवरण देखें",
  "REPEAT_RIDE": "रिपीट राइड",
  "FARE_WAS_HIGH": "किराया अधिक था",
  "AUTO_ASSIGN_DRIVER": "ड्राइवर को ऑटो-असाइन करें",
  "CHOOSE_BETWEEN_MULTIPLE_DRIVERS": "एकाधिक ड्राइवरों के बीच चुनें",
  "CHOOSE_BETWEEN_MULTIPLE_RIDES": "एक से अधिक राइड के बीच चुनें",
  "ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE": "अपनी राइड का चयन करने के लिए इस विशेषता को सक्षम करें",
  "BOOKING_PREFERENCE": "बुकिंग वरीयता",
  "BASE_FARES": "बेस किराया",
  "PICKUP_CHARGE": "चालक पिकअप शुल्क",
  "TOTAL_PAID": "पूर्ण भुगतान",
  "WAITING_CHARGE": "प्रतीक्षा प्रभार**",
  "NOMINAL_FARE": "वैकल्पिक ड्राइवर का अनुरोध*",
  "DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO": "* ड्राइवर वैकल्पिक रूप से ट्रैफ़िक, वापसी यात्रा की संभावना आदि जैसे अन्य कारकों को कवर करने के लिए बेस फेयर के 10% (निकटतम रु. 10 तक) का अनुरोध कर सकते हैं।",
  "WAITING_CHARGE_DESCRIPTION": "** प्रतीक्षा शुल्क: ₹1 / मिनट - ड्राइवर पार्टनर के आने के 3 मिनट बाद",
  "SUCCESSFUL_ONBOARD": "आपने नम्मा यात्री में सफलतापूर्वक साइन इन कर लिया है",
  "HAVE_REFERRAL_CODE": "रेफरल कोड है?",
  "REFEREAL_CODE_DISCRIPTION": "आपका रेफ़रल किसी ड्राइवर को पुरस्कृत किए जाने \n का कारण हो सकता है!",
  "SIX_DIGIT_REFERRAL_CODE": "ड्राइवर द्वारा साझा किया गया 6 अंकों का कोड दर्ज करें",
  "ABOUT_REFERRAL_PROGRAM": "रेफरल प्रोग्राम क्या है?",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "रेफ़रल कार्यक्रम ड्राइवरों को अधिक सवारी स्वीकार करने,सवारी कम रद्द करने और योग्य ड्राइवरों को पहचानने और पुरस्कृत करके आपको बेहतर सेवा देने के लिए प्रोत्साहित करता है। \nआप ड्राइवर का रेफरल कोड डालकर मदद कर सकते हैं और नम्मा यात्री समुदाय के लिए सवारी की गुणवत्ता में सुधार कर सकते हैं!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nआप अपने नम्मा यात्री ड्राइवर से पूछकर एक रेफरल कोड प्राप्त कर सकते हैं।",
  "REFERRAL_CODE_SUCCESSFULL": "आपने रेफ़रल कोड सफलतापूर्वक लागू कर दिया है!",
  "REFERRAL_CODE_APPLIED": "रेफ़रल लागू!",
  "HEY": "नमस्ते",
  "INVALID_CODE_PLEASE_RE_ENTER": "अवैध कोड। कृपया पुनः दर्ज करें",
  "LET_TRY_THAT_AGAIN": "आइए फिर से कोशिश करें...",
  "CONTACTS_SELECTED": "संपर्क चयनित",
  "SELECT_CONTACTS": "संपर्क चुनें",
  "CONFIRM_EMERGENCY_CONTACTS": "आपके आपातकालीन संपर्क की पुष्टि करें",
  "MAXIMUM_CONTACTS_LIMIT_REACHED": "अधिकतम संपर्क सीमा पार कर गया",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT": "क्या आप वाकई उन्हें अपने आपातकालीन संपर्कों से हटाना चाहते हैं?",
  "SEARCH_CONTACTS": "संपर्क खोजें",
  "SELECTED_CONTACT_IS_INVALID": "चयनित संपर्क अमान्य है। कृपया एक वैध संपर्क चुनें।",
  "CALL_EMERGENCY_CONTACTS": "आपातकालीन संपर्कों को कॉल करें",
  "LIVE_STATS_DASHBOARD": "लाइव आँकड़े डैशबोर्ड",
  "CHECK_OUT_LIVE_STATS": "लाइव आंकड़े देखें",
  "ADD_ANOTHER_CONTACT": "अन्य संपर्क जोड़ें",
  "EMERGENCY_CONTACS_ADDED_SUCCESSFULLY": "आपातकालीन संपर्क सफलतापूर्वक जोड़े गए हैं।",
  "NO_CONTACTS_FOUND_ON_DEVICE_TO_ADD": "जोड़ने के लिए कोई संपर्क नहीं मिला।",
  "NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD": "जोड़ने के लिए कोई संपर्क नहीं बचा है।",
  "PERMISSION_DENIED": "अनुमति नहीं दी गई",
  "PERCENTAGE_OF_NOMINAL_FARE": "~ बेस फेयर का 10%",
  "PAY_VIA_CASH_OR_UPI": "नकद / यूपीआई के माध्यम से भुगतान करें",
  "BOARD_THE_FIRST_TAXI": "यात्री साथी अंचल से पहली टैक्सी में सवार हों",
  "REQUEST_CALLBACK": "कॉलबैक का अनुरोध करें",
  "BOARD_THE_FIRST_TAXI": "यात्री साथी अंचल से पहली टैक्सी में सवार हों",
  "CHOOSE_YOUR_RIDE": "अपनी सवारी चुनें",
  "PAY_VIA_CASH_OR_UPI": "नकद / यूपीआई के माध्यम से भुगतान करें",
  "ECONOMICAL": "किफ़ायती",
  "COMFY": "आरामदायक",
  "NAVIGATE": "नेविगेट",
  "GOVERNMENT_CHAGRES": "सरकारी शुल्क",
  "SERVICE_CHARGES": "सेवा शुल्क",
  "CONFIRM_AND_BOOK": "पुष्टि करें और बुक करें",
  "PEOPLE": "लोग",
  "MAKE_YOURSELF_AVAILABLE_FOR": "निम्नलिखित के लिए स्वयं को उपलब्ध कराएं",
  "SPACIOUS": "विशाल",
  "EASY_ON_WALLET": "किफ़ायती",
  "UPTO": "तक",
  "SEARCH_AGAIN_WITH_A_TIP": "टिप के साथ फिर से खोजें?",
  "TRY_AGAIN_WITH_A_TIP": "टिप के साथ पुन: प्रयास करें?",
  "BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS": "ऐसा लगता है कि यह बहुत ही व्यस्त दिन है। सवारी मिलने की संभावना बढ़ाने के लिए आप बख्शीश जोड़ने की कोशिश कर सकते हैं।",
  "TRY_AGAIN_WITHOUT_TIP": "टिप के बिना पुन: प्रयास करें",
  "SEARCH_AGAIN_WITHOUT_A_TIP": "टिप के बिना फिर से खोजें",
  "TRY_AGAIN_WITH": "दुबारा कोशिश करें",
  "SEARCH_AGAIN_WITH": "दोबारा सर्च करे",
  "TIP": "टिप",
  "START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS": "इन त्वरित चैट सुझावों का उपयोग करके अपनी चैट प्रारंभ करें",
  "MESSAGE": "संदेश",
  "I_AM_ON_MY_WAY": "मैं रास्ते में हूं|",
  "GETTING_DELAYED_PLEASE_WAIT": "देरी हो रही है। प्रतीक्षा करें|",
  "UNREACHABLE_PLEASE_CALL_BACK": "पहुंच योग्य नहीं है, वापस कॉल करें।",
  "ARE_YOU_STARING": "क्या आप आ रहे हैं?",
  "PLEASE_COME_SOON": "कृपया जल्दी आ जाएँ|",
  "OK_I_WILL_WAIT": "मैं इंतज़ार करूंगा|",
  "I_HAVE_ARRIVED": "मैं पहूंच गया हूं|",
  "PLEASE_COME_FAST_I_AM_WAITING": "जल्दी आइए, मैं इंतजार कर रहा हूं।",
  "PLEASE_WAIT_I_WILL_BE_THERE": "कृपया प्रतीक्षा करें, मैं आ रहा हूँ|",
  "LOOKING_FOR_YOU_AT_PICKUP": "मैं पिकअप पर हूँ|",
  "START_YOUR_CHAT_WITH_THE_DRIVER": "ड्राइवर के साथ अपनी चैट शुरू करें|",
  "MOBILE": "मोबाइल",
  "HOW_DO_YOU_IDENTIFY_YOURSELF": "आप अपने आप को कैसे पहचानते हैं?",
  "SELECT_YOUR_GENDER": "अपना लिंग चुनें",
  "FEMALE": "महिला",
  "MALE": "नर",
  "PREFER_NOT_TO_SAY": "चुप रहना पसंद करूंगा",
  "EMAIL_ID": "ईमेल आईडी",
  "SET_NOW": "अभी सेट करें",
  "ADD_NOW": "अभी जोड़ें",
  "HOW_SHOULD_WE_ADDRESS_YOU": "हमें आपको कैसे संबोधित करना चाहिए?",
  "GENDER_STR": "लिंग",
  "PROFILE_COMPLETION": "प्रोफ़ाइल पूर्णता",
  "EARLY_END_RIDE_CHARGES": "जल्दी सवारी खत्म करने का शुल्क^",
  "EARLY_END_RIDE_CHARGES_DESCRIPTION": "^जल्दी सवारी खत्म करने पर अतिरिक्त शुल्क लगता है जो तय नहीं की गई दूरी का आधा किराया होता है (अधिकतम ₹50)।",
  "CANCEL_ONGOING_SEARCH": "क्या आप वाकई चल रही खोज को रद्द करना जारी रखना चाहते हैं?",
  "YES_TRY_AGAIN": "हाँ, पुन: प्रयास करें",
  "NO_DONT": "नहीं",
  "YES_CANCEL_SEARCH": "हाँ, खोज रद्द करें",
  "TRY_LOOKING_FOR_RIDES_AGAIN": "ऐसा लगता है कि यह बहुत ही व्यस्त दिन है। आप फिर से राइड खोजने की कोशिश कर सकते हैं",
  "NO_TIP": "0 टिप",
  "CUSTOMER_SELECTED_FARE": "ग्राहक टिप^",
  "CUSTOMER_TIP_DESCRIPTION": "^राइड मिलने की संभावना बढ़ाने के लिए ग्राहक द्वारा जोड़ी गई अतिरिक्त राशि।",
  "PLACE_CALL": "फोन करें",
  "DIRECT_CALL": "प्रत्यक्ष कॉल",
  "ANONYMOUS_CALL": "अनाम कॉल",
  "YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE": "आपका नंबर ड्राइवर पार्टनर को नहीं दिखाया जाएगा। अनुपालन के लिए कॉल रिकॉर्ड की जाएगी।",
  "YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER": "आपका नंबर ड्राइवर को दिखाई देगा। अगर रजिस्टर्ड नंबर से कॉल नहीं कर रहे हैं तो इस्तेमाल करें",
  "CALL_DRIVER_USING": "ड्राइवर को कॉल करें",
  "WAS_YOUR_CALL_SUCCESSFUL": "क्या आपका कॉल सक्सेसफुल था",
  "DRIVER_ADDITIONS": "चालक जोड़*",
  "FARE_UPDATE_POLICY": "किराया अपडेट नीति",
  "DRIVER_ADDITIONS_OPTIONAL": "चालक जोड़ (वैकल्पिक)",
  "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "चालक यातायात को कवर करने, वापसी यात्रा की संभावना आदि के लिए अतिरिक्त बोली लगा सकता है",
  "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ड्राइवर द्वारा अतिरिक्त सीमा की गणना बेस फेयर के 10% पर की जाती है, जिसे निकटतम ₹10 में राउंड किया जाता है",
  "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE": "ध्यान दें: ड्राइवर पार्टनर यह अतिरिक्त किराया चार्ज कर सकते हैं या नहीं भी कर सकते हैं।",
  "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS": "आप नीचे दिए गए किसी भी कारण से अपडेटेड अंतिम किराया देख सकते हैं:",
  "REASON_CHANGE_IN_ROUTE_A": "1.मार्ग में परिवर्तन: ",
  "REASON_CHANGE_IN_ROUTE_B": "मार्ग में बदलाव के कारण कुल किराए में बदलाव हो सकता है",
  "NAVIGATE_TO_PICKUP" : "पिकअप पर नेविगेट करें",
  "REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON" : "अनुरोध प्राप्त हुआ है। हम जल्द ही आपसे संपर्क करेंगे",
  "CONTACT_REMOVED_SUCCESSFULLY" : "संपर्क सफलतापूर्वक निकाला गया",
  "CORPORATE_ADDRESS" : "कॉर्पोरेट पता",
  "CORPORATE_ADDRESS_DESCRIPTION" : "जसपे टेक्नोलॉजीज प्राइवेट लिमिटेड <br> गिरिजा बिल्डिंग, नंबर 817, गणपति मंदिर रोड, 8वां ब्लॉक, कोरमंगला, बेंगलुरु, कर्नाटक 560095, भारत।",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "वेबसाइट: <u>https://nammayatri.in/</u>",
  "REGISTERED_ADDRESS" : "पंजीकृत पता",
  "REGISTERED_ADDRESS_DESCRIPTION" : "जस्पे टेक्नोलॉजीज प्राइवेट लिमिटेड <br> स्टैलियन बिजनेस सेंटर, नंबर 444, तीसरी और चौथी मंजिल, 18वीं मेन, 6वां ब्लॉक, कोरमंगला बेंगलुरु, कर्नाटक- 560095, भारत।",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "वेबसाइट: <u>https://nammayatri.in/</u>",
  "RECOMMENDED" : "अनुशंसित",
  "COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE" : "वैयक्तिकृत सवारी अनुभव के लिए अपनी प्रोफ़ाइल पूर्ण करें",
  "UPDATE_NOW" : "अभी अपडेट करे",
  "WE_WOULD_APPRECIATE_YOUR_FEEDBACK" : "खाता हटाने के पीछे आपके तर्क की हम सराहना करेंगे",
  "REASON_FOR_DELETING_ACCOUNT" : "खाता हटाने का कारण*",
  "SUBMIT_REQUEST" : "अनुरोध सबमिट करें",
  "PLEASE_ENTER_A_VALID_EMAIL" : "कृपया एक मान्य ईमेल दर्ज करें",
  "WE_WOULD_APPRECIATE_YOUR_REASONING" : "खाता हटाने के पीछे आपके तर्क पर हम आपकी प्रतिक्रिया की सराहना करेंगे",
  "OK_GOT_IT" : "ठीक, समझ गया",
  "WAIT_FOR_DRIVER" : "ड्राइवर की प्रतीक्षा करें",
  "NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS" : "मुझे दूसरी ऐप पर सवारी मिल चुकी है।",
  "CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP" : "मुझे दूसरी ऐप पर सवारी मिल चुकी है।",
  "DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP" : "ड्राइवर की लोकेशन मैप पर नहीं बदल रही थी।",
  "DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION" : "ड्राइवर को पिकअप लोकेशन तक पहुंचने में काफी समय लग रहा था।",
  "THE_PICKUP_LOCATION_ENTERED_WAS_WRONG" : "दर्ज किया गया पिकअप स्थान गलत था।",
  "YOUR_DRIVER_IS_JUST" : "आपका ड्राइवर बस",
  "M_AWAY" : " m दूर है।",
  "DRIVER_HAS_ALREADY_TRAVELLED" : "ड्राइवर ",
  "PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING" : "\nकृपया कैंसिल करने से पहले ड्राइवर से संपर्क करें।",
  "CHANGE_OF_PLANS" : "प्लान में परिवर्तन",
  "DRIVER_IS_NOT_MOVING" : "ड्राइवर एक स्थान पर ठहरा हुआ है।",
  "WRONG_PICKUP_LOCATION" : "गलत पिकअप स्थान",
  "DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE" : "हो सकता है ड्राइवर किसी और रास्ते से आ रहा हो।",
  "DRIVER_IS_NOT_MOVING_Q" : "कया आपका ड्राइवर स्थिर है?",
  "WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING" : "\nक्या आप कैंसिल करने से पहले ड्राइवर से बात करना चाहेंगे?",
  "DRIVER_IS_NEAR_YOUR_LOCATION" : "ड्राइवर आपकी लोकशन पर पहुँच चुका है।",
  "HAS_TRAVELLED" : " का सफर तय कर चुका है।",
  "SOME_OTHER_REASON" : "कोई और कारण।",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "नम्मा यात्री में आपका स्वागत है! \nराइड बुक करना शुरू करने के लिए, हमें आपके डिवाइस की लोकेशन की आवश्यकता होती है।",
  "METRO_RIDE" : "मेट्रो की सवारी",
  "GO_BACK_TEXT": "पीछे जाएं",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST": "ड्राइवर ने आपके विशेष अनुरोध को प्राथमिकता दी और वह केवल ",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST": "ड्राइवर ने आपके विशेष अनुरोध को प्राथमिकता दी और पहले ही ",
  "AND_HAS_TRAVELLED": " की यात्रा कर चुका है।",
  "PLEASE_FIND_REVISED_FARE_ESTIMATE": "कृपया संशोधित अनुमानित किराया ज्ञात करें। रात का शुल्क दिन के शुल्क का 1.5 गुना है।",
  "NAVIGATE_TO_PICKUP" : "पिकअप पर नेविगेट करें" ,
  "FARE_ESTIMATE" : "किराया अनुमान" ,
  "TIP_SELECTED" : "टिप चयनित",
  "ADD_A_TIP_TO_FIND_A_RIDE_QUICKER" : "तेजी से सवारी खोजने के लिए एक टिप जोड़ें!",
  "IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL" : "ऐसा लगता है कि इसमें सामान्य से ज़्यादा समय लग रहा है।",
  "CONTINUE_SEARCH_WITH" : "के साथ खोज जारी रखें",
  "CONTINUING_SEARCH_WITH" : "से तलाश जारी है",
  "SEARCHING_WITH" : "से खोज रहे हैं",
  "THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "ड्राइवर ने आपके विशेष अनुरोध को प्राथमिकता दी और वह आपके स्थान के रास्ते पर है।",
  "DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "ड्राइवर आपके स्थान के रास्ते पर है।",
  "ALLOW_LOCATION_ACCESS": "स्थान पहुंच की अनुमति दें",
  "MESSAGE_FROM_DRIVER": "ड्राइवर का संदेश",
  "REPLY": "जवाब",
  "NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS" : "नाम 2 अक्षर से अधिक होना चाहिए",
  "THIS_FIELD_IS_REQUIRED" : "यह फ़ील्ड आवश्यक है",
  "EMAIL_EXISTS_ALREADY" : "ईमेल पहले से ही मौजूद है",
  "OKAY_GOT_IT" : "ठीक है समझ आ गया",
  "CALL_NAMMA_YATRI_SUPPORT" : "नम्मा यात्री सहायता को कॉल करें",
  "CALL_112": "112 पर कॉल करें",
  "CALL_EMERGENCY_CENTRE": "आपातकालीन केंद्र को कॉल करें",
  "DESCRIPTION_SHOULD_BE_MORE_THAN_10_CHARACTERS" :"विवरण 10 अक्षरों से अधिक होना चाहिए",
  "THIS_FIELD_IS_REQUIRED" : "यह फ़ील्ड आवश्यक है", 
  "EMAIL_EXISTS_ALREADY" : "ईमेल पहले से ही मौजूद है",
  "PLATFORM_FEE" : "प्लेटफार्म शुल्क",
  "THIS_FIELD_IS_REQUIRED" : "यह फ़ील्ड आवश्यक है", 
  "EMAIL_EXISTS_ALREADY" : "ईमेल पहले से ही मौजूद है",
  "PLATFORM_FEE" : "प्लेटफ़ॉर्म शुल्क",
  "SGST" : "एसजीएसटी"
}