export function getMerchantString(key) {
  var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return getStringHIValue(key);
    case "BN_IN":
      return getStringBNValue(key);
    default:
      return getStringENValue(key);
  }
}

export function isMerchantString(key) {
  return config.StringKeys.includes(key);
}

export function getMerchantConfig(key){
  if (key in config){
      return config[key];
  }
  console.error("no value found for key "+ key);
  return "";
}

export function getStringENValue(key) {
  if (key in englishStrings) {
    return englishStrings[key];
  }
  return "error in getCommonEN";
}

export function getStringHIValue(key) {
  if (key in hindiStrings) {
    return hindiStrings[key];
  }
  return "error in getCommonHI";
}

export function getStringBNValue(key) {
  if (key in bengaliStrings) {
    return bengaliStrings[key];
  }
  return "error in getCommonBN";
}


const config = {
  "StringKeys": ["ABOUT_APP_DESCRIPTION",
  "WELCOME_TEXT",
  "REQUEST_AUTO_RIDE",
  "CURRENTLY_WE_ARE_LIVE_IN_",
  "DRIVER_PICKUP_CHARGES",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT",
  "SUCCESSFUL_ONBOARD", "ABOUT_REFERRAL_PROGRAM_DISCRIPTION",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL",
  "CALL_NAMMA_YATRI_SUPPORT", 
  "YOUR_FEEDBACK_HELPS_US"],

  "isReferralEnabled" : "false",
  "showBookingPreference" : "false",
  "showRateCard" : "false",
  "showDashboard" : "false",
  "enableShareRide" : "false",
  "shareAppTitle" : "Share Jatri Sathi!",
  "shareAppContent" : "Hey there!\n\n Jatri Sathi is Coming Soon",
  "DOCUMENT_LINK" : "https://docs.google.com/document/d/19pQUgTWXBqcM7bjy4SU1-z33r-iXsdPMfZggBTXbdR4",
  "APP_LINK" : "",
  "PRIVACY_POLICY_LINK" : "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E",
  "CUSTOMER_TIP" : "false",
  "ABOUT_US_LOGO" : "ic_launcher,https://assets.juspay.in/beckn/jatrisaathi/user/images/ic_launcher.png"
}

const bengaliStrings = {
  "ABOUT_APP_DESCRIPTION": "যাত্রী সাথী হল চালকদের সাথে রাইডারদের সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি রাইডারদের জন্য মিটার রেট সহ একটি রাইড বুক করা সুবিধাজনক করে তোলে তাই ন্যূনতম ভাড়া।",
  "WELCOME_TEXT": "স্বাগতম সাথী",
  "REQUEST_AUTO_RIDE": "যাত্রার অনুরোধ করুন",
  "CURRENTLY_WE_ARE_LIVE_IN_": "বর্তমানে আমরা কলকাতায় থাকি, আপনি সেখানে আমাদের পরিষেবা উপভোগ করতে পারেন",
  "DRIVER_PICKUP_CHARGES": "সারভিস চারজ",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "আপনি যাত্রী সাথী সমর্থন দলকে একটি কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
  "SUCCESSFUL_ONBOARD": "আপনি সফলভাবে যাত্রী সাথীতে \n স্বাক্ষর করেছেন",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "রেফারেল প্রোগ্রাম চালকদের আরও রাইড গ্রহণ করতে, কম বাতিল করতে এবং যোগ্য ড্রাইভারদের চিনতে এবং পুরস্কৃত করার মাধ্যমে আপনাকে আরও ভাল পরিবেশন করতে উত্সাহিত করে। \n\n আপনি ড্রাইভারের রেফারেল কোড লিখে সাহায্য করতে পারেন এবং যাত্রী সাথী সম্প্রদায়ের জন্য রাইডের মান উন্নত করতে পারেন!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n আপনি আপনার যাত্রী সাথী ড্রাইভারকে জিজ্ঞাসা করে একটি রেফারেল কোড পেতে পারেন।",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "আপনার অবস্থান আমাদের সিস্টেমকে ট্যাক্সি দ্বারা আশেপাশের সমস্ত স্থান ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড করতে সাহায্য করে৷",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "আপনি জাত্রি সাথি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Jatri Saathi-এ স্বাগতম \nরাইড বুকিং শুরু করতে, অনুগ্রহ করে আমাদের আপনাকে খুঁজে পেতে অনুমতি দিন!",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "YOUR_FEEDBACK_HELPS_US" : "আপনার প্রতিক্রিয়া আমাদের যাত্রী সাথী অভিজ্ঞতা উন্নত করতে সাহায্য করে",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "CALL_NAMMA_YATRI_SUPPORT" : "সহযাত্রী সমর্থন কল করুন"
}

const hindiStrings = {
  "ABOUT_APP_DESCRIPTION": "जात्री साथी सवारियों को चालकों से जोड़ने का एक खुला मंच है। ऐप राइडर्स के लिए मीटर रेट के साथ राइड बुक करना सुविधाजनक बनाता है इसलिए न्यूनतम किराया।",
  "WELCOME_TEXT": "स्वागत है साथी",
  "REQUEST_AUTO_RIDE": "राइड का अनुरोध करें",
  "CURRENTLY_WE_ARE_LIVE_IN_": "वर्तमान में हम कोलकाता में रहते हैं, आप वहां हमारी सेवाओं का आनंद ले सकते हैं",
  "DRIVER_PICKUP_CHARGES": "सेवा प्रभार",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
  "SUCCESSFUL_ONBOARD": "आपने जात्री साथी पर सफलतापूर्वक हस्ताक्षर कर \n लिए हैं",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "रेफ़रल कार्यक्रम ड्राइवरों को अधिक सवारी स्वीकार करने, कम रद्द करने और योग्य ड्राइवरों को पहचानने और पुरस्कृत करके आपको बेहतर सेवा देने के लिए प्रोत्साहित करता है। \n\n आप ड्राइवर का रेफ़रल कोड डालकर मदद कर सकते हैं और जात्री साथी समुदाय के लिए सवारी की गुणवत्ता में सुधार कर सकते हैं!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n आप अपने जात्री साथी ड्राइवर से पूछकर रेफ़रल कोड प्राप्त कर सकते हैं।",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस पास के सभी टैक्सियों को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "जात्री साथी में आपका स्वागत है! \nसवारी की बुकिंग शुरू करने के लिए, कृपया हमें आपको ढूंढने की अनुमति दें!",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "YOUR_FEEDBACK_HELPS_US" : "आपकी प्रतिक्रिया हमें जात्री साथी अनुभव को बेहतर बनाने में मदद करती है",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "CALL_NAMMA_YATRI_SUPPORT" : "साथी यात्री सहायता को कॉल करें"
}

const englishStrings = {
  "ABOUT_APP_DESCRIPTION": "Jatri Sathi is an open platform to connect riders with drivers. The app makes it convenient for riders to book a ride with meter rate hence minimal fare.",
  "WELCOME_TEXT": "Welcome to Jatri Sathi",
  "REQUEST_AUTO_RIDE": "Request Ride",
  "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kolkata, you can enjoy our services there",
  "DRIVER_PICKUP_CHARGES": "Service Charges",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "You are about to place a call to the Jatri Sathi Support Team. Do you want to proceed?",
  "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Jatri Sathi",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Jatri Sathi Community!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Jatri Sathi Driver.",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "You are about to place a call to the Jatri Sathi Support Team. Do you want to proceed?",
  "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Jatri Sathi! \nTo start booking rides, please allow us to find you!",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri Sathi experience",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "CALL_NAMMA_YATRI_SUPPORT" : "Call Yatri Sathi Support"
}