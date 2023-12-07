import { callbackMapper } from "presto-ui";

const { JOS, JBridge } = window;

const Android = window.Android;
let timerIdForTimeout;
const allTimerIID = [];
let uniqueId = 0;
let countDownInMinutesId = null;
const microapps = ["in.juspay.hyperpay", "in.juspay.ec", "in.juspay.upiintent"];

export const generateUniqueId = function (unit) {
  uniqueId += 1;
  return JSON.stringify(uniqueId);
};

export const getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};


function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  allTimerIID.push(window.timerId);
  return window.timerId;
}

export const getTimer = function (valType) {
  return function (startTime) {
    return function (inputVal) {
      return function (cb){
        return function (action) {
          return function(){
            const callback = callbackMapper.map(function () {

              console.log(valType+"<===>"+startTime+"<===>"+inputVal)

              const t = new Date().getTime()

              console.log("ttt-->>"+t)
              let countDownDate = (new Date(inputVal).getTime())-5000;

              console.log("countDownDate-->>"+countDownDate)

              if((Math.floor(((countDownDate - t) % (1000 * 60)) / 1000)) > 25000)
              {
                countDownDate = (new Date().getTime())+ 20000;
              }

              instantGetTimer (function() {

                // Get today's date and time
                const now = new Date().getTime();

                // Find the distance between now and the count down date
                const distance = countDownDate - now;

                // Time calculations for days, hours, minutes and seconds
                const days = Math.floor(distance / (1000 * 60 * 60 * 24));
                const hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
                const minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
                const seconds = Math.floor((distance % (1000 * 60)) / 1000);

                // Display the result in the element with id="demo"
                // document.getElementById("demo").innerHTML = days + "d " + hours + "h "
                // + minutes + "m " + seconds + "s ";
                // If the count down is finished, write some text
                // console.log("Distance -->>")
                // console.log(distance)
                // console.log("-------------")
                if (valType == "STOP"){
                  clearInterval(window.timerId);
                }

                if (distance <= 0) {
                  clearInterval(window.timerId);

                  console.log("EXPIRED");
                  cb(action("EXPIRED"))();
                  // return "EXPIRED";
                }else{
                  let timer;
                  if(days==0){
                    if(hours == 0){
                      if(minutes==0){
                        timer = (seconds+1) + "s "
                      }else{
                        timer = minutes + "m " + seconds + "s "
                      }
                    }else{
                      timer = hours + "h "+ minutes + "m " + seconds + "s "
                    }
                  }else{
                    timer = days + "d " + hours + "h "+ minutes + "m " + seconds + "s "
                  }

                  console.log(timer);
                  // return timer;
                  cb(action(timer))();
                }
              }, 1000);
              console.log("timerId",window.timerId);
            });
            // return JBridge.timerCallback(callback);
            window.callUICallback(callback);
          }

        }
      }
    }
  }
}

export const clampNumber = function (number) {
  return function(originalMax) {
    return function(newMax) {
      const originalMin = 0;
      const newMin = 0;
      const originalRange = originalMax - originalMin;
      const newRange = newMax - newMin;
      
      const percentage = (number - originalMin) / originalRange;
      
      return Math.floor(newMin + percentage * newRange);
    }
  }
}

export const get15sTimer = function (cb){
  return function (action) {
    return function(){
      const callback = callbackMapper.map(function () {
        let seconds = 15;
        instantGetTimer (function() {
          seconds = seconds - 1;
          if (seconds <= 0) {
            clearInterval(window.timerId);
            console.log("EXPIRED");
            cb(action("EXPIRED"))();
          }else{
            const timer = seconds + "s "
            console.log(timer);
            cb(action(timer))();
          }
        }, 1000);
        console.log("timerId",window.timerId);
      });
      window.callUICallback(callback);
    }

  }
}

export const get5sTimer = function (cb){
  return function (action) {
    return function(){
      function sayLetter(seconds){
        if (seconds >= 0)
        {
          console.log(seconds+ "seconds")
          setTimeout(() => {
            if (seconds <= 0) {
              clearInterval(window.timerId);
              console.log("EXPIRED");
              cb(action("EXPIRED"))();
            }else{
              const timer = seconds + "s "
              console.log(timer);
              cb(action(timer))();
              sayLetter(seconds-1);
            }
          }, 1000);
        }
      }
      const callback = callbackMapper.map(function () {
        const time = 5;
        sayLetter(time);
        console.log("timerId",window.timerId);
      });
      window.callUICallback(callback);
    }
  }
}

export const parseNumber = function (num) {
  num = num.toString();
  let lastThree = num.substring(num.length-3);
  const otherNumbers = num.substring(0,num.length-3);
  if(otherNumbers != "")
    lastThree = "," + lastThree;
  const res = otherNumbers.replace(/\B(?=(\d{2})+(?!\d))/g, ",") + lastThree;
  return res;
}


export const get10sTimer = function (cb){
  return function (action) {
    return function(){
      const callback = callbackMapper.map(function () {
        function sayLetter(seconds){
          if (seconds >= 0)
          {
            console.log(seconds+ "seconds")
            setTimeout(() => {
              if (seconds <= 0) {
                clearInterval(window.timerId);
                console.log("EXPIRED");
                cb(action("EXPIRED"))();
              }else{
                const timer = seconds + "s "
                console.log(timer);
                cb(action(timer))();
                sayLetter(seconds-1);
              }
            }, 1000);
          }
        }
        const time = 10;
        sayLetter(time);
        console.log("timerId",window.timerId);
      });
      window.callUICallback(callback);
    }
  }
}

export const startTimer = function(input) {
  return function (isCountDown) {
    return function (cb) {
      return function (action) {
        return function () {
          const callback = callbackMapper.map(function () {
            let time = input;
            function startCountDown(seconds) {
              if (seconds >= 0) {
                timerIdForTimeout = setTimeout(() => {
                  if (seconds <= 0) {
                    clearTimeout(timerIdForTimeout);
                    console.log("EXPIRED");
                    console.log(seconds + "seconds");
                    cb(action("EXPIRED"))();
                  } else {
                    const timer = seconds + "s ";
                    console.log(timer + "seconds");
                    cb(action(timer))();
                    startCountDown(seconds + (isCountDown ? -1 : 1));
                  }
                }, 1000);
              }
            }
            time = time + (isCountDown ? -1 : 1);
            startCountDown(time);
            console.log("inside startTimer");
            console.log("timerId : " + timerIdForTimeout);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}


export const clearTimer = function (a)
{ if(timerIdForTimeout){
  clearTimeout(timerIdForTimeout);
}
if(window.timerId){
  clearInterval(window.timerId);
}
};


export const clearAllTimer = function(a) {
  console.log("allTimerIID");
  console.log(allTimerIID);
  while(allTimerIID.length > 0){
    clearInterval(parseInt(allTimerIID.pop()));
  }
}
export const setRefreshing = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      const cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

export const setEnabled = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      const cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

export const decodeErrorCode = function (a) {
  try {
    const errorCodee = JSON.parse(a).errorCode;
    return  errorCodee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

export const decodeErrorMessage = function (a) {
  try {
    const errorMessagee = JSON.parse(a).errorMessage;
    if(errorMessagee === null)
    {
      return "";
    }
    return  errorMessagee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

export const convertKmToM = function (dist) {
  try{
    const distance = parseInt(dist);
    if (distance<1000)
    {
      return ((distance.toString()) + "m");
    }
    else
    {
      const disKm = distance/1000;
      return (((disKm.toFixed(1)).toString()) + "km");
    }
  }catch(e){
    console.log(e);
    console.log("error in convertKmToM----------------------------------");
  }
};

export const differenceBetweenTwoUTC = function (str1) {
  return function (str2) {
    const curr1 = new Date(str1);
    const departure = new Date(str2);
    console.log(departure + " , " + curr1 + "STR");
    let diff =(curr1.getTime() - departure.getTime())/ 1000;
    diff = (Math.round(diff));
    return diff
  };
};


export const storeCallBackForNotification = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (notificationType) {
          cb(action(notificationType))();
        });
        window.onResumeListeners = [];
        JBridge.storeCallBackForNotification(callback);
      }
      catch (error){
        console.log("Error occurred in storeCallBackForNotification ------", error);
      }
    }
  }
}

export const getcurrentdate = function (string) {
  let today = new Date();
  const dd = String(today.getDate()).padStart(2, "0");
  const mm = String(today.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = today.getFullYear();

  today = yyyy + "-" + mm + "-" + dd;
  return today;
}

export const getDatebyCount = function (count) {
  let today = new Date();
  today.setDate(today.getDate() - count);
  const dd = String(today.getDate()).padStart(2, "0");
  const mm = String(today.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = today.getFullYear();

  today = yyyy + "-" + mm + "-" + dd;
  return today;
}

export const hideSplash = JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()

export const currentPosition = function (str) {
  return function() {
    JBridge.currentPosition(str);
  }
}

export const toStringJSON = function (attr) {
  return JSON.stringify(attr);
};

export const getTime = function (unit){
  return Date.now();
}

export const toInt = function (val) {
  return parseInt(val);
}

export const secondsLeft = function (time){
  const validity = new Date(time).getTime();
  const now = new Date().getTime();
  if (validity <= now){
    return parseInt(1);
  }else{
    return parseInt((validity - now)/1000);
  }
}

export const objectToAllocationType = function (stringifyObject) {
  const json = JSON.parse(stringifyObject);
  console.log(json);
  return json;
}

export const storeCallBackTime = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (time, lat, lng) {
          cb(action(time)(lat)(lng))();
        });
        JBridge.storeCallBackTime(callback);
      }
      catch (error){
        console.log("Error occurred in storeCallBackTime ------", error);
      }
    }
  }
}

export const launchAppSettings = function (unit) {
  return JBridge.launchAppSettings();
};

export const shuffle = function (array) {
  const shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

export const getTimeStampString = function (utcTime){
  const createdDate = new Date(utcTime);
  const currentDate = new Date();
  const diff = (currentDate.getTime() - createdDate.getTime())/ 1000;
  const seconds = (Math.round(diff));
  if (seconds <0) return "";
  const d = Math.floor(seconds / (3600*24));
  const h = Math.floor(seconds % (3600*24) / 3600);
  const m = Math.floor(seconds % 3600 / 60);
  const s = Math.floor(seconds % 60);
  if      (d > 0) return d + (d == 1 ? " day " : " days")
  else if (h > 0) return h + (h == 1 ? " hour " : " hours")
  else if (m > 0) return m + (m == 1 ? " minute " : " minutes")
  else            return s + (s == 1 ? " second" : " seconds")
}

export const clearFocus = function (id) {
  return JBridge.clearFocus(id)
}

export const addMediaPlayer = function (id) {
  return function(source) {
    return function () {
      JBridge.addMediaPlayer(id,source);
    }
  };
};

export const saveAudioFile = function (source) {
  return function() {
    return JBridge.saveAudioFile(source);
  }
}

export const uploadMultiPartData = function (path) {
  return function (url) {
    return function(fileType) {
      return function() {
        return JBridge.uploadMultiPartData(path, url, fileType);
      }
    }
  }
}

export const startAudioRecording = function (id) {
  return function() {
    return JBridge.startAudioRecording();
  }
};

export const stopAudioRecording = function (id) {
  return function() {
    return JBridge.stopAudioRecording();
  }
}

export const renderBase64ImageFile = function (base64Image) {
  return function(id) {
    return function (fitCenter) {
      return function (imgScaleType){
        return function () {
          try{
            return JBridge.renderBase64ImageFile(base64Image, id, fitCenter, imgScaleType);
          }catch (err){
            return JBridge.renderBase64ImageFile(base64Image, id, fitCenter);
          }
        }
      }  
    }
  }
}

export const removeMediaPlayer = function (id) {
  return function () {
    JBridge.removeMediaPlayer();
  };
};

export const getPeriod = function(date) {
  const currentDate = new Date();
  const pastDate = new Date(date);
  const diff = Math.floor(currentDate.getTime() - pastDate.getTime());
  const days = Math.floor(diff/(1000 * 60 * 60 * 24));
  const months = Math.floor(days/31);
  const years = Math.floor(months/12);
  const period = years > 0 ? years : months > 0 ? months : days > 0 ? days : 0
  const periodType = years > 0 ? "Yrs" : months > 0 ? "Months" : days > 0 ? "Days" : "new"
  return {period : Math.abs(period)
    , periodType : periodType}
}

export const getMerchantId = function(id) {
  return window.merchantID;
}

function getRoot(rootObject) {
  return rootObject ? rootObject.default.root : "";
}

function getFileName(url) {
  const reg = /(v1-[^/]*\.((zip)|(jsa)))|[^/]*\.(html|js)/;
  const out = reg.exec(url);
  return out ? out[0] : url;
}

function constructDownloadObject (root,url) {
  const download = {};
  Object.assign(download,({
    "filePath" : root.concat(getFileName(url)),
    "location" : url
  }));
  return download;
}

function getDownloadObject(assets,bundles,apps,dependencies) {
  const files = [];
  apps.forEach((app) => {

    if (!bundles[app]) return;
    const assetsBlock = assets[app];
    const root = getRoot(dependencies[app]);
    const assetsKey = Object.keys(assetsBlock);
    const assetsList = [];



    assetsKey.forEach((key) => {if (typeof assetsBlock[key] == "string") assetsList.push(assetsBlock[key])})

    files.push(constructDownloadObject(root,bundles[app]));

    assetsList.forEach((url) => {
      files.push(constructDownloadObject(root,url));
    });
  });
  return files;
}

export const preFetch = function() {
  let configPackage = {};
  try {
    if (top.configPackage) configPackage = Object.assign({},top.configPackage)
    else configPackage = JSON.parse(JBridge.loadFileInDUI("config.json"));
  }catch (err){
    window.JBridge.firebaseLogEventWithParams("pre_fetch_failed", "config_read_failed",err);
  }

  const JOSFlags = JOS.getJOSflags();
  let assets = {};
  let bundles = {};
  if (JOSFlags && JOSFlags.isCUGUser) {
    assets = Object.assign(assets,configPackage.new.assets)
    bundles = Object.assign(bundles,configPackage.new.package)
  } else {
    assets = Object.assign(assets,configPackage.live.assets)
    bundles = Object.assign(bundles,configPackage.live.package)
  }
  return getDownloadObject(assets,bundles,configPackage.app_list,configPackage.dependencies);
}

// TODO NEED TO REFACTOR 
export const renewFile = function(filePath,fileLocation,cb) {
  JBridge.renewFile(fileLocation,filePath,callbackMapper.map(function(result) {
    cb(result)();
  }));
}

export const isYesterday = function (dateString){
  try {
    const yesterday = new Date()
    yesterday.setDate(yesterday.getDate() - 1);
    const date = new Date(dateString);
    return (yesterday.toDateString() == date.toDateString());
  }catch(error){
    console.error(error);
  }
  return false;
}

export const isDateGreaterThan = function(dateString){
  try {
    const currDate = new Date();
    const futureDate = new Date(dateString);
    return currDate > futureDate;
  }catch(error){
    console.error("error", error);
  }
  return false;
}

export const getPopupObject = function (just, nothing, key){
  try {
    const val = JBridge.getFromSharedPrefs(key);
    if (val == "__failed") {
      return nothing;
    } 
    return just(JSON.parse(val));
  } catch( e ){
    console.warn(e);
  }
  return nothing;
}


export const getDateAfterNDays = function (n) {
  const today = new Date();
  const dateAfterNDays = new Date(today);
  dateAfterNDays.setDate(today.getDate() + n);
  const year = dateAfterNDays.getFullYear();
  const month = String(dateAfterNDays.getMonth() + 1).padStart(2, "0");
  const day = String(dateAfterNDays.getDate()).padStart(2, "0");
  const result = `${day}/${month}/${year}`;
  return `${day}/${month}/${year}`;
}


export const _generateQRCode = function (data, id, size, margin, sc) {
  if (typeof JBridge.generateQRCode === "function") {
    try {
      const cb = callbackMapper.map(function (_status) {
        console.log("QR status:: ", _status);
        sc(_status)();
      });
      JBridge.generateQRCode(data, id, size, margin, cb);
    } catch (e) {
      console.warn(e);
      sc("FAILURE")();
    }
  }
  else {
    sc("FAILURE")();
  }
}

export const downloadQR = function (id){
  if (window.JBridge.downloadLayoutAsImage)
    return window.JBridge.downloadLayoutAsImage(id);
}


export const getPixels = function (){
  if (window.parent.devicePixelRatio) {
    return window.parent.devicePixelRatio;
  } else {
    return window.JBridge.getPixels();
  }
}
export const getDeviceDefaultDensity = function (){
  if (window.JBridge.getSessionInfo) {
    const sessionInfo = JSON.parse(window.JBridge.getSessionInfo())
    return sessionInfo.screen_ppi;
  } else {
    return window.JBridge.getDensity() * 160;
  }
}

export const countDownInMinutes = function (seconds, cb, action) {
  let sec;
  function convertInMinutesFormat() {
    sec = sec - 60;
    const minutes = Math.floor(sec / 60);
    cb(action(countDownInMinutesId)(minutes + 1)(sec))();
  }
  try {
    sec = seconds - 5; //5 seconds buffer
    if (sec <= 0) {
      cb(action(countDownInMinutesId)("")(0))();
    } else {
      if (countDownInMinutesId) clearInterval(countDownInMinutesId);
      countDownInMinutesId = setInterval(convertInMinutesFormat, 60000); //setting interval of 1 min
      cb(action(countDownInMinutesId)(Math.floor(sec / 60) + 1)(sec))();
    }
  } catch (error) {
    console.error("Error occured ", error);
  }
}

export const istToUtcDate = function (dateStr) {
  try {
    const dateObj = new Date(dateStr);
    const offsetMilliseconds = 5 * 60 * 60 * 1000 + 30 * 60 * 1000;
    const newDateObj = new Date(dateObj.getTime() - offsetMilliseconds);
    return newDateObj.toISOString();
  } catch (err) {
    console.error("Error in istToUtcDate " + err);
  }
};

export const setValueToLocalStore = function (key,value){
  JBridge.setInSharedPrefs(key, value);
}
