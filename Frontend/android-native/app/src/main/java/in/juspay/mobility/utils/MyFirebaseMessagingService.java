/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import static in.juspay.mobility.utils.NotificationUtils.rand;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.pushnotification.NotificationInfo;
import com.clevertap.android.sdk.pushnotification.fcm.CTFcmMessageHandler;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;
import org.json.JSONObject;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.TimeZone;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.net.ssl.HttpsURLConnection;
import in.juspay.mobility.CommonJsInterface;

import in.juspay.mobility.BuildConfig;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;
import com.google.firebase.analytics.FirebaseAnalytics;

public class MyFirebaseMessagingService extends FirebaseMessagingService {
    private FirebaseAnalytics mFirebaseAnalytics;
    String merchantType = BuildConfig.MERCHANT_TYPE;

    private RideRequestUtils rideRequestUtils = new RideRequestUtils();

    public static HashMap<String, Long> clearedRideRequest = new HashMap<>();

    @Override
    public void onNewToken(@NonNull String newToken){
        super.onNewToken(newToken);
        Log.e("newToken", newToken);
        String deviceToken = newToken;
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        CleverTapAPI.getDefaultInstance(this).pushFcmRegistrationId(newToken,true);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString("FCM_TOKEN", newToken);
        editor.apply();
        String regToken = sharedPref.getString("REGISTERATION_TOKEN", "null");
        if (!regToken.equals("null") && !regToken.equals("__failed")) {
            updateFCMToken(deviceToken);
        }
        Log.e("newToken", newToken);
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    @Override
    public void onMessageReceived(@NonNull RemoteMessage remoteMessage){
        firebaseLogEventWithParams("notification_received","type",remoteMessage.getData().get("notification_type"));
        super.onMessageReceived(remoteMessage);
        Log.e("onMessageReceived", remoteMessage.getData().toString());
        JSONObject payload = new JSONObject();
        JSONObject notification_payload = null;
        JSONObject entity_payload = null;
        try {
            if (remoteMessage.getData().size() > 0) {
                Bundle extras = new Bundle();
                for (Map.Entry<String, String> entry : remoteMessage.getData().entrySet()) {
                    extras.putString(entry.getKey(), entry.getValue());
                }

                CleverTapAPI.getDefaultInstance(this).pushNotificationViewedEvent(extras);

                NotificationInfo info = CleverTapAPI.getNotificationInfo(extras);

                if (info.fromCleverTap) {
                    new CTFcmMessageHandler().createNotification(getApplicationContext(), remoteMessage);
                } else {

                    payload.put("notification_type", remoteMessage.getData().get("notification_type"));
                    payload.put("entity_ids", remoteMessage.getData().get("entity_ids"));
                    payload.put("entity_type", remoteMessage.getData().get("entity_type"));
                    payload.put("show_notification", remoteMessage.getData().get("show_notification"));

                    String title;
                    String body;
                    String icon;
                    String imageUrl;

                    if (remoteMessage.getData().containsKey("notification_json")) {
                        notification_payload = new JSONObject(remoteMessage.getData().get("notification_json"));
                    }
                    if (remoteMessage.getData().containsKey("entity_data") && remoteMessage.getData().get("notification_type").equals("NEW_RIDE_AVAILABLE")) {
                        entity_payload = new JSONObject(remoteMessage.getData().get("entity_data"));
                    }

                    RemoteMessage.Notification notification = remoteMessage.getNotification();
                    if (notification != null) {
                        title = notification.getTitle();
                        body = notification.getBody();
                        icon = notification.getIcon();
                        imageUrl = remoteMessage.getData().get("image-url");
                    } else {
                        title = notification_payload.get("title").toString();
                        body = notification_payload.get("body").toString();
                        icon = notification_payload.get("icon").toString();
                        if (notification_payload.has("imageUrl")) {
                            imageUrl = notification_payload.get("imageUrl").toString();
                        } else {
                            imageUrl = null;
                        }
                    }

                    if (notification_payload != null || notification != null) {
                        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                        String notificationType = (String) payload.get("notification_type");

                        stopChatService(notificationType, sharedPref);
                        switch (notificationType) {
                            case NotificationTypes.DRIVER_NOTIFY:
                                if (remoteMessage.getData().containsKey("driver_notification_payload")) {
                                    showOverlayMessage(new JSONObject(remoteMessage.getData().get("driver_notification_payload")));
                                }
                                break;

                            case NotificationTypes.TRIGGER_SERVICE:
                                if (merchantType.equals("DRIVER")) {
                                    FirebaseAnalytics.getInstance(this).logEvent("notification_trigger_service", new Bundle());
                                    rideRequestUtils.restartLocationService(this);
                                }
                                break;

                            case NotificationTypes.NEW_RIDE_AVAILABLE:
                                sharedPref.edit().putString(getString(R.string.RIDE_STATUS), getString(R.string.NEW_RIDE_AVAILABLE)).apply();
                                if (sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") && (sharedPref.getString("ACTIVITY_STATUS", "null").equals("onPause") || sharedPref.getString("ACTIVITY_STATUS", "null").equals("onDestroy")) || sharedPref.getString("IS_VALID_TIME","true").equals("false")) {
                                    startWidgetService(null, payload, entity_payload);
                                } else {
                                    NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                                }
                                break;

                            case NotificationTypes.CLEARED_FARE:
                                sharedPref.edit().putString(getString(R.string.CLEAR_FARE), String.valueOf(payload.get(getString(R.string.entity_ids)))).apply();
                                NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                                startWidgetService("CLEAR_FARE", payload, entity_payload);
                                break;

                            case NotificationTypes.CANCELLED_PRODUCT:
                                NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                                if (sharedPref.getString("MAPS_OPENED", "null").equals("true")) {
                                    startMainActivity();
                                } else {
                                    startWidgetService(getString(R.string.ride_cancelled), payload, entity_payload);
                                }
                                break;

                            case NotificationTypes.DRIVER_QUOTE_INCOMING:
                                if (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause")) {
                                    NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                }
                                break;

                            case NotificationTypes.TRIP_FINISHED:
                                NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                if (merchantType.equals("USER")) {
                                    sharedPref.edit().putInt("RIDE_COUNT", sharedPref.getInt("RIDE_COUNT", 0) + 1).apply();
                                    sharedPref.edit().putString("COMPLETED_RIDE_COUNT", String.valueOf(sharedPref.getInt("RIDE_COUNT", 0))).apply();
                                }
                                break;

                            case NotificationTypes.DRIVER_ASSIGNMENT:
                                NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "true").apply();
                                sharedPref.edit().putString(getString(R.string.RIDE_STATUS), getString(R.string.DRIVER_ASSIGNMENT)).apply();
                                startMainActivity();
                                break;

                            case NotificationTypes.BUNDLE_UPDATE:
                                try {
                                    if (MainActivity.getInstance() != null) {
                                        MainActivity.getInstance().showAlertForUpdate();
                                    } else {
                                        firebaseLogEventWithParams("unable_to_update_bundle", "reason", "Main Activity instance is null");
                                    }
                                } catch (Exception e) {
                                    e.printStackTrace();
                                    firebaseLogEventWithParams("exception_in_bundle_update _fcm", "exception", e.toString());
                                }
                                break;

                            case NotificationTypes.CANCELLED_SEARCH_REQUEST:
                                sharedPref.edit().putString(getString(R.string.CANCELLED_SEARCH_REQUEST), String.valueOf(payload.get(getString(R.string.entity_ids)))).apply();
                                NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                                startWidgetService("CLEAR_FARE", payload, entity_payload);
                                break;

                            case NotificationTypes.NEW_MESSAGE:
                                sharedPref.edit().putString("ALERT_RECEIVED", "true").apply();
                                NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                break;

                            case NotificationTypes.REGISTRATION_APPROVED:
                                sharedPref.edit().putString(getString(R.string.REGISTRATION_APPROVED), "true").apply();
                                break;

                            case NotificationTypes.REFERRAL_ACTIVATED:
                                sharedPref.edit().putString("REFERRAL_ACTIVATED", "true").apply();
                                break;

                            case NotificationTypes.UPDATE_STORAGE:
                                if (notification_payload.has("storage_key") && notification_payload.has("storage_value")) {
                                    String storage_key = notification_payload.get("storage_key").toString();
                                    String storage_value = notification_payload.get("storage_value").toString();
                                    if (storage_key.equals("update_driver_status") && merchantType.equals("DRIVER")) {
                                        boolean status = storage_value.equals("SILENT") || storage_value.equals("ONLINE");
                                        rideRequestUtils.updateDriverStatus(status, storage_value, this, true);
                                    } else
                                        sharedPref.edit().putString(storage_key, storage_value).apply();
                                }
                                NotificationUtils.showAllocationNotification(this, title, body, payload, imageUrl, entity_payload);
                                break;

                            case NotificationTypes.CALL_API:
                                try {
                                    String endPoint = notification_payload.get("endpoint").toString();
                                    String method = notification_payload.get("method").toString();
                                    JSONObject reqBody = (JSONObject) notification_payload.get("reqBody");
                                    if (endPoint != null && method != null) {
                                        reqBody = reqBody != null ? reqBody : null;
                                        rideRequestUtils.callAPIViaFCM(endPoint, reqBody, method, this);
                                    }

                        }catch (Exception e) {
                            e.printStackTrace();
                        }
                    case NotificationTypes.CHAT_MESSAGE :
                        try{
                            String appState = "";
                            if(sharedPref != null) appState = sharedPref.getString("ACTIVITY_STATUS", "null");
                            if((appState.equals("onDestroy") || appState.equals("onPause"))) {
                                NotificationUtils.createChatNotification(title,body,getApplicationContext());
                            }
                        } catch (Exception e) {
                            Log.e("MyFirebaseMessagingService", "Error in CHAT_MESSAGE " + e);
                        }
                        break;

                            case NotificationTypes.REALLOCATE_PRODUCT:
                                try {
                                    if (sharedPref.getString("REALLOCATE_PRODUCT_ENABLED", "false").equals("false"))
                                        break;
                                    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                                    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                                    String getCurrTime = dateFormat.format(new Date());
                                    sharedPref.edit().putString(getString(R.string.FINDING_QUOTES_START_TIME), getCurrTime).apply();
                                    sharedPref.edit().putString(getString(R.string.LOCAL_STAGE), getString(R.string.ReAllocated)).apply();
                                    NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                    sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "false").apply();
                                } catch (Exception e) {
                                    e.printStackTrace();
                                }
                                break;
                            default:
                                if (payload.get("show_notification").equals("true")) {
                                    NotificationUtils.showNotification(this, title, body, payload, imageUrl);
                                } else {
                                    // Silent notification
                                }
                                break;
                        }
                    }
                }
            }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception_in_notification","remoteMessage",remoteMessage.getData().toString());
            return;
        }
    }

    private void showOverlayMessage(JSONObject payload) {
        try{
            Intent showMessage = new Intent(getApplicationContext(), OverlayMessagingService.class);
            showMessage.putExtra("payload", payload.toString());
            startService(showMessage);
        }catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void updateFCMToken( final String deviceToken){
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        String baseUrl = sharedPref.getString("BASE_URL", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
                StringBuilder result = new StringBuilder();
                try {
                    String orderUrl ;
                    if (merchantType.equals("DRIVER")) {
                        orderUrl = baseUrl + "/driver/profile";
                    }
                    else {
                        orderUrl = baseUrl + "/profile";
                        }
                    System.out.print("in updateFCMToken");
                    HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                    if (connection instanceof HttpsURLConnection)
                        ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                    connection.setRequestMethod("POST");
                    connection.setRequestProperty("Content-Type", "application/json");
                    connection.setRequestProperty("token", token);
                    connection.setRequestProperty("x-device", deviceDetails);
                    connection.setDoOutput(true);

                    JSONObject payload = new JSONObject();
                    payload.put("deviceToken", deviceToken);

                    OutputStream stream = connection.getOutputStream();
                    stream.write(payload.toString().getBytes());
                    connection.connect();
                    int respCode = connection.getResponseCode();
                    InputStreamReader respReader;

                    if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                        respReader = new InputStreamReader(connection.getErrorStream());
                        System.out.print("in error : " + respReader);
                    } else {
                        respReader = new InputStreamReader(connection.getInputStream());
                        System.out.print("in 200 : " + respReader);
                    }

                    BufferedReader in = new BufferedReader(respReader);
                    String inputLine;

                    while ((inputLine = in.readLine()) != null) {
                        result.append(inputLine);
                    }
                    System.out.print("in result : " + result.toString());

                } catch (Exception ignored) {
                    System.out.println("Catch in updateFCMToken : " +ignored);
                }
                handler.post(()->{
                    onDestroy();
                    stopForeground(true);
                    stopSelf();
                    executor.shutdown();
                });
            });
    }

    private void startWidgetService(String widgetMessage, JSONObject data, JSONObject payload){
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        Intent widgetService = new Intent(getApplicationContext(), WidgetService.class);
        if (merchantType.equals("DRIVER") && Settings.canDrawOverlays(getApplicationContext())  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            widgetService.putExtra(getResources().getString(R.string.WIDGET_MESSAGE),widgetMessage);
            widgetService.putExtra("payload", payload!=null ? payload.toString(): null);
            widgetService.putExtra("data", data!=null ? data.toString(): null);
            try{
                startService(widgetService);
            }catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void startMainActivity() {
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if (merchantType.equals("DRIVER") && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            Intent intent = new Intent(getApplicationContext(), MainActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            try {
                getApplicationContext().startActivity(intent);
            }catch (Exception e){
                firebaseLogEventWithParams("exception" , "startMainActivity", e.toString());
            }
        }
    }

    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    private void stopChatService(String notificationType, SharedPreferences sharedPref){
        if(notificationType.equals("TRIP_FINISHED") || notificationType.equals("CANCELLED_PRODUCT") || notificationType.equals("TRIP_STARTED") || notificationType.equals("REALLOCATE_PRODUCT")) {
            try{
                Intent chatListenerService = new Intent(this, ChatService.class);
                this.stopService(chatListenerService);
                SharedPreferences.Editor editor = sharedPref.edit();
                editor.putString("READ_MESSAGES", "0");
                editor.apply();
                Intent overlayService = new Intent(getApplicationContext(), MessageOverlayService.class);
                getApplicationContext().stopService(overlayService);
            } catch (Exception e){
                Log.e("MyFirebaseMessagingService", "Error in stopChatService : " + e);
            }
        }
    }

    private class NotificationTypes {
        private static final String TRIGGER_SERVICE = "TRIGGER_SERVICE";
        private static final String NEW_RIDE_AVAILABLE = "NEW_RIDE_AVAILABLE";
        private static final String CLEARED_FARE = "CLEARED_FARE";
        private static final String CANCELLED_PRODUCT = "CANCELLED_PRODUCT";
        private static final String DRIVER_QUOTE_INCOMING = "DRIVER_QUOTE_INCOMING";
        private static final String TRIP_FINISHED = "TRIP_FINISHED";
        private static final String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT";
        private static final String BUNDLE_UPDATE = "BUNDLE_UPDATE";
        private static final String CANCELLED_SEARCH_REQUEST = "CANCELLED_SEARCH_REQUEST";
        private static final String NEW_MESSAGE = "NEW_MESSAGE";
        private static final String REGISTRATION_APPROVED = "REGISTRATION_APPROVED";
        private static final String REFERRAL_ACTIVATED = "REFERRAL_ACTIVATED";
        private static final String UPDATE_STORAGE = "UPDATE_STORAGE";
        private static final String CALL_API = "CALL_API";
        private static final String CHAT_MESSAGE = "CHAT_MESSAGE";
        private static final String DRIVER_NOTIFY = "DRIVER_NOTIFY";
        private static final String REALLOCATE_PRODUCT = "REALLOCATE_PRODUCT";
    }
}