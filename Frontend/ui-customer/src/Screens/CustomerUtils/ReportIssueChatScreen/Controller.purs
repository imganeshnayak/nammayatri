{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.Controller where

import Prelude

import Components.AddAudioModel.Controller as AddAudioModel
import Components.AddImagesModel.Controller as AddImagesModel
import Components.ChatView (Action(..), ChatComponent) as ChatView
import Components.ChatView.Controller (Action(..), makeChatComponent, makeChatComponent') as ChatView
import Components.PrimaryButton.Controller (Action(..)) as PrimaryButton
import Components.PrimaryEditText.Controller (Action(..)) as PrimaryEditText
import Components.RecordAudioModel.Controller (Action(..)) as RecordAudioModel
import Components.ViewImageModel.Controller as ViewImageModel
import Data.Array (deleteAt, length, snoc)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Engineering.Helpers.Commons (getNewIDWithTag, getCurrentUTC)
import JBridge (addMediaFile, clearWaitingTimer, hideKeyboardOnNavigation, scrollToEnd, startLottieProcess, toast, uploadFile, lottieAnimationConfig, clearFocus, removeMediaPlayer, renderBase64ImageFile, saveAudioFile, startAudioRecording, stopAudioRecording, uploadMultiPartData, waitingCountdownTimer)
import Data.String.CodeUnits (stripSuffix)
import Data.String.Pattern (Pattern(Pattern))
import Data.TraversableWithIndex (forWithIndex)
import Effect.Class (liftEffect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender)
import PrestoDOM.Types.Core (class Loggable, Eval)
import PrestoDOM.Utils (continue, continueWithCmd, exit)
import Screens (ScreenName(..), getScreen)
import Screens.Types (ReportIssueChatScreenState)
import Types.EndPoint as EndPoint

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REPORT_ISSUE_CHAT_SCREEN)
    BackPressed -> do
                   trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
                   trackAppEndScreen appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    Exit _ -> do
                   trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
                   trackAppEndScreen appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    NoAction -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "no_action"
    SubmitIssue -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "submit_issue"
    ShowOptions -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "show_options"
    AddImage -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "add_image"
    AddAudio _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "add_audio"
    DeleteRecordedAudio -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "delete_recorded_audio"
    DeleteSelectedImages -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "delete_selected_images"
    CancelCall _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "cancel_call"
    ConfirmCall _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "confirm_call"
    AddAudioModelAction (AddAudioModel.OnClickDone _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_audio_model" "on_click_done"
    AddAudioModelAction (AddAudioModel.OnClickCross)-> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_audio_model" "on_click_cancel"
    AddAudioModelAction AddAudioModel.OnClickDelete -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_audio_model" "on_click_delete"
    AddAudioModelAction AddAudioModel.AddAudio -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_audio_model" "add_audio"
    AddAudioModelAction AddAudioModel.BackPressed -> trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    AddAudioModelAction AddAudioModel.NoAction -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_audio_model" "no_action"
    AddImagesModelAction (AddImagesModel.OnClickDone _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "on_click_done"
    AddImagesModelAction (AddImagesModel.OnClickCancel) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "on_click_cancel"
    AddImagesModelAction (AddImagesModel.OnClickDelete _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "on_click_delete"
    AddImagesModelAction (AddImagesModel.OnClickView _ _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "on_click_view"
    AddImagesModelAction (AddImagesModel.AddImage) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "add_image"
    AddImagesModelAction (AddImagesModel.BackPressed) -> trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    AddImagesModelAction (AddImagesModel.NoAction) -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_images_model" "no_action"
    ImageUploadCallback _ _ _ -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "image_upload_callback"
    ViewImageModelAction (ViewImageModel.BackPressed) -> trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    ViewImageModelAction (ViewImageModel.NoAction) -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_view_image_model" "no_action"
    RecordAudioModelAction (RecordAudioModel.OnClickDone) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_record_model" "on_click_done"
    RecordAudioModelAction (RecordAudioModel.OnClickRestart) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_record_model" "on_click_restart"
    RecordAudioModelAction (RecordAudioModel.OnClickClose) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_record_model" "on_click_close"
    RecordAudioModelAction (RecordAudioModel.OnClickStop) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_record_model" "on_click_stop"
    RecordAudioModelAction (RecordAudioModel.BackPressed) -> trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    RecordAudioModelAction (RecordAudioModel.NoAction) -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_record_model" "no_action"
    UpdateRecordModelPlayer _ -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "update_record_model_player"
    ChatViewActionController (ChatView.SendMessage) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_chat_view" "send_message"
    ChatViewActionController (ChatView.SendSuggestion _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_chat_view" "send_suggestion"
    ChatViewActionController (ChatView.BackPressed) -> trackAppBackPress appId (getScreen REPORT_ISSUE_CHAT_SCREEN)
    ChatViewActionController (ChatView.OnImageClick _) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_chat_view" "on_image_click"
    ChatViewActionController (ChatView.EnableSuggestions) -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_chat_view" "enable_suggestions"
    SendMessage _ _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "send_message"
    UpdateState _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "update_state"
    PrimaryEditTextActionController _ -> trackAppActionClick appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "primary_edit_text_changed"
    _ -> trackAppScreenEvent appId (getScreen REPORT_ISSUE_CHAT_SCREEN) "in_screen" "other_action"

instance showAction :: Show Action where
    show _ = ""

data Action = Exit (ScreenOutput)
            | NoAction
            | BackPressed
            | AfterRender
            | SubmitIssue
            | ShowOptions
            | AddImage
            | AddAudio Boolean
            | DeleteRecordedAudio
            | DeleteSelectedImages
            | CancelCall  PrimaryButton.Action
            | ConfirmCall PrimaryButton.Action
            | AddAudioModelAction  AddAudioModel.Action
            | AddImagesModelAction AddImagesModel.Action
            | ImageUploadCallback  String String String
            | UploadMultiPartDataCallback  String String
            | ViewImageModelAction ViewImageModel.Action
            | RecordAudioModelAction   RecordAudioModel.Action
            | UpdateRecordModelPlayer  String
            | ChatViewActionController ChatView.Action
            | SendMessage ChatView.ChatComponent Boolean
            | UpdateState ReportIssueChatScreenState
            | PrimaryEditTextActionController PrimaryEditText.Action
            | ReopenIssuePress
            | KeyboardCallback String

data ScreenOutput = GoBack
                  | UploadIssue  ReportIssueChatScreenState
                  | CallDriver ReportIssueChatScreenState
                  | CallSupport ReportIssueChatScreenState
                  | SelectIssueOption ReportIssueChatScreenState
                  | ReopenIssue ReportIssueChatScreenState

eval :: Action -> ReportIssueChatScreenState -> Eval Action ScreenOutput ReportIssueChatScreenState

eval ReopenIssuePress state = 
  exit $ ReopenIssue state

eval (Exit output) state =
  exit output

eval (KeyboardCallback keyBoardState) state = do 
  let keyState = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false 
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state {props {isKeyboardOpen = keyState}}

eval BackPressed state =
  continueWithCmd state [do
    _  <- removeMediaPlayer ""
    _ <- pure $ hideKeyboardOnNavigation true
    pure $ Exit GoBack
  ]

eval AfterRender state =
  continue state

eval SubmitIssue state =
  exit $ UploadIssue state

eval ShowOptions state = do
  let options'  = map (\x -> x.option) state.data.options
  continue state { data { chatConfig { enableSuggestionClick = false, suggestionsList = options' } } }

---------------------------------------------------- Add Media ----------------------------------------------------
eval AddImage state =
  if length state.data.addedImages > 0
  then
    continueWithCmd state { props { showImageModel = true, isPopupModelOpen = true }
                   , data  { addImagesState { images = state.data.addedImages, stateChanged = false } } } [do
                     _ <- pure $ clearFocus (getNewIDWithTag "submit_chat_edit_text")
                     pure NoAction
                   ]
  else
    continueWithCmd state { props { showImageModel = true, isPopupModelOpen = true }
                          , data  { addImagesState { images = state.data.addedImages, stateChanged = false } } } [do
      _ <- pure $ clearFocus (getNewIDWithTag "submit_chat_edit_text")
      void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "primary_button_loader.json", lottieId = (getNewIDWithTag "add_images_model_done_button"), scaleType = "CENTER_CROP" }
      pure NoAction
    ]

eval (AddAudio stateChangedState) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ clearFocus (getNewIDWithTag "submit_chat_edit_text")
  if isJust state.data.recordedAudioUrl
  then
    continue state { props { showAudioModel = true, isPopupModelOpen = true }
                   , data  { addAudioState { audioFile = state.data.recordedAudioUrl, stateChanged = stateChangedState } } }
  else
    continue state { props { showRecordModel = true, isPopupModelOpen = true }
                   , data { recordAudioState { recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00:00" } } }

---------------------------------------------------- Delete Media ----------------------------------------------------
eval DeleteRecordedAudio state =
  continueWithCmd state { data { recordedAudioUrl = Nothing, uploadedAudioId = Nothing, addAudioState { audioFile = Nothing }, recordAudioState { recordedFile = Nothing } } } [do
    _ <- removeMediaPlayer ""
    pure $ NoAction
  ]

eval DeleteSelectedImages state =
  continue state { data { addedImages = [], uploadedImagesIds = [], addImagesState { images = [], imageMediaIds = [] } } }

---------------------------------------------------- Add Audio Model ----------------------------------------------------
eval (AddAudioModelAction AddAudioModel.AddAudio) state =
  continueWithCmd state { props { showAudioModel = false } } [do
    pure $ UpdateState state { props { showAudioModel = false, showRecordModel = true }
                             , data  { recordAudioState { recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = true, isUploading = false, timer = "00:00" } } }
  ]

eval (AddAudioModelAction AddAudioModel.BackPressed) state = do
  continueWithCmd state [do
    if state.props.isPopupModelOpen
    then pure $ (AddAudioModelAction (AddAudioModel.OnClickCross))
    else pure $ BackPressed
  ]

eval (AddAudioModelAction (AddAudioModel.OnClickCross)) state =
  continueWithCmd state { props { showAudioModel = false, isPopupModelOpen = false }
                        , data  { recordAudioState { recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00:00" } } } [do
    _ <- removeMediaPlayer ""
    pure NoAction
  ]

eval (AddAudioModelAction (AddAudioModel.OnClickDone PrimaryButton.OnClick)) state =
  continueWithCmd state { props { showAudioModel = false, isPopupModelOpen = false } } [do
    _ <- removeMediaPlayer ""
    pure $ UpdateState state { props { showAudioModel = false, isPopupModelOpen = false }, data { recordedAudioUrl = Nothing, uploadedAudioId = Nothing } }
  ]

eval (AddAudioModelAction (AddAudioModel.OnClickDelete)) state =
  continueWithCmd state { data { addAudioState { audioFile = Nothing, stateChanged = true }, recordAudioState { recordedFile = Nothing } } } [do
    _ <- removeMediaPlayer ""
    pure $ NoAction
  ]

---------------------------------------------------- Add Image Model ----------------------------------------------------
eval (AddImagesModelAction (AddImagesModel.AddImage)) state =
  continueWithCmd state [do
    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "primary_button_loader.json", lottieId = (getNewIDWithTag "add_images_model_done_button"), scaleType = "CENTER_CROP" }
    _ <- liftEffect $ uploadFile unit
    pure NoAction
  ]

eval (AddImagesModelAction (AddImagesModel.OnClickView image imageName)) state =
  continue state { data  { viewImageState { image = image, imageName = Just imageName } }
                 , props { showViewImageModel = true } }

eval (AddImagesModelAction (AddImagesModel.OnClickDone PrimaryButton.OnClick)) state =
  continue state { data  { uploadedImagesIds = state.data.addImagesState.imageMediaIds, addedImages = state.data.addImagesState.images }
                 , props { showImageModel = false, isPopupModelOpen = false } }

eval (AddImagesModelAction (AddImagesModel.OnClickCancel)) state =
  continue state { props { showImageModel = false, isPopupModelOpen = false }
                 , data  { addImagesState { imageMediaIds = state.data.uploadedImagesIds, images = state.data.addedImages } } }

eval (AddImagesModelAction (AddImagesModel.OnClickDelete index)) state = do
  let images'   = fromMaybe state.data.addImagesState.images        $ deleteAt index state.data.addImagesState.images
  let imageIds' = fromMaybe state.data.addImagesState.imageMediaIds $ deleteAt index state.data.addImagesState.imageMediaIds
  continueWithCmd state { data { addImagesState { images = images', stateChanged = not (imageIds' == state.data.uploadedImagesIds), imageMediaIds = imageIds' } } } [do
    _ <- forWithIndex images' \i x -> do
      _ <- renderBase64ImageFile x.image (getNewIDWithTag "add_image_component_image" <> (show i)) false "CENTER_CROP"
      pure NoAction
    pure NoAction
  ]

eval (AddImagesModelAction AddImagesModel.BackPressed) state = do
  continueWithCmd state [do
    if state.props.isPopupModelOpen
    then pure (AddImagesModelAction (AddImagesModel.OnClickCancel))
    else pure BackPressed
  ]

---------------------------------------------------- Timer Callback ----------------------------------------------------
eval (RecordAudioModelAction (RecordAudioModel.TimerCallback timerID timeInMinutes seconds)) state = 
  continue state { data { recordAudioState { timer = timeInMinutes } }, props {timerId = timerID} }

---------------------------------------------------- Add Image Callback ----------------------------------------------------
eval (ImageUploadCallback image imageName imagePath) state = do
  let images' = if length state.data.addImagesState.imageMediaIds == 3
                then do
                  pure $ toast (getString MAX_IMAGES)
                  state.data.addImagesState.images
                else
                  snoc state.data.addImagesState.images { image, imageName }
  continueWithCmd state { data { addImagesState { images = images',  isLoading = true } } } [do
    _ <- uploadMultiPartData imagePath (EndPoint.uploadFile "") "Image"
    pure NoAction
  ]

eval (UploadMultiPartDataCallback fileType fileId) state = do
  if (fileType == "Image") 
    then do
      let uploadedImagesIds' = if length state.data.addImagesState.imageMediaIds == 3
                                then do
                                  state.data.addImagesState.imageMediaIds
                                else
                                  snoc state.data.addImagesState.imageMediaIds fileId
      continue state { data { addImagesState {isLoading = false, stateChanged = true, imageMediaIds = uploadedImagesIds' } } }
    else do
      continueWithCmd state { data  { uploadedAudioId = Just fileId }, props { isPopupModelOpen = false, showRecordModel = false } } [do
        _ <- removeMediaPlayer ""
        pure $ NoAction
      ]

---------------------------------------------------- View Image Model ----------------------------------------------------
eval (ViewImageModelAction (ViewImageModel.BackPressed)) state = do
  if state.props.showViewImageModel
  then
    continue state { data  { viewImageState { image = "", imageName = Nothing } }
                   , props { showViewImageModel = false } }
  else
    continueWithCmd state [do
      if state.props.showImageModel
      then pure $ (AddImagesModelAction AddImagesModel.BackPressed)
      else pure $ BackPressed
    ]
---------------------------------------------------- Record Audio Model ----------------------------------------------------
eval (RecordAudioModelAction (RecordAudioModel.OnClickRecord push)) state = do
   continueWithCmd state { data { recordAudioState { timer = "00 : 00" } } }  [do
    cond <- startAudioRecording ""
    if cond 
    then do
      _ <- pure $ clearWaitingTimer state.props.timerId
      _ <- removeMediaPlayer ""
      _ <- waitingCountdownTimer 0 push RecordAudioModel.TimerCallback
      pure $ UpdateState state { data { recordAudioState { isRecording = true, timer = "00 : 00" } } }
    else
      pure $ NoAction
  ]

eval (RecordAudioModelAction RecordAudioModel.OnClickStop) state =
  continueWithCmd state { data { recordAudioState { isRecording = false, recordingDone = true, timer = "00 : 00" } } } [do
    _   <- pure $ clearWaitingTimer state.props.timerId
    res <- stopAudioRecording ""
    pure $ UpdateRecordModelPlayer res
  ]

eval (RecordAudioModelAction RecordAudioModel.OnClickDone) state =
  continueWithCmd state { data { recordAudioState { isUploading = true } } } [do
    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "audio_upload_animation.json", lottieId = (getNewIDWithTag "audio_recording_done"), scaleType = "FIT_CENTER", speed = 1.0 }
    void $ pure $ clearWaitingTimer state.props.timerId
    case state.data.recordAudioState.recordedFile of
      Just url -> do
                  res <- saveAudioFile url
                  _ <- uploadMultiPartData res (EndPoint.uploadFile "") "Audio"
                  pure $  UpdateState state { data  { recordedAudioUrl = Just res}}
      Nothing  -> do
                  if state.data.recordAudioState.openAddAudioModel
                  then do
                    _ <- removeMediaPlayer ""
                    pure $ UpdateState state { props { showRecordModel = false, showAudioModel = true }
                                             , data  { addAudioState { stateChanged = true } } }
                  else
                    pure $ UpdateState state { data  { recordedAudioUrl = Nothing } }
  ]

eval (RecordAudioModelAction RecordAudioModel.OnClickRestart) state =
  continueWithCmd state [do
    _ <- removeMediaPlayer ""
    pure $ UpdateState state { data { recordAudioState { isRecording = false, recordingDone = false } } }
  ]

eval (RecordAudioModelAction RecordAudioModel.OnClickClose) state =
  if state.data.recordAudioState.openAddAudioModel
  then
    continueWithCmd state { props { showRecordModel = false }, data { recordAudioState { recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00 : 00" } } } [do
      _ <- removeMediaPlayer ""
      _ <- stopAudioRecording ""
      _ <- pure $ clearWaitingTimer state.props.timerId
      pure $ UpdateState state { props { showRecordModel = false, showAudioModel = true, isPopupModelOpen = true }
                   , data { recordAudioState { recordedFile = Nothing, recordingDone = false, isRecording = false, openAddAudioModel = false, isUploading = false, timer = "00 : 00" }, addAudioState { audioFile = Nothing, stateChanged = true } } }
    ]
  else
    continue state { data  { recordAudioState { recordedFile = Nothing } }
                   , props { isPopupModelOpen = false, showRecordModel = false } }

eval (RecordAudioModelAction RecordAudioModel.BackPressed) state = do
  continueWithCmd state [do
    if state.props.isPopupModelOpen
    then pure (RecordAudioModelAction RecordAudioModel.OnClickClose)
    else pure BackPressed
  ]

eval (UpdateRecordModelPlayer url) state = do
  continueWithCmd state { data { recordAudioState { recordedFile = Just url } } } [do
    _ <- addMediaFile (getNewIDWithTag "recordedAudioViewUniqueOne") url (getNewIDWithTag "actionButtonRecord") "ny_ic_play_recorded_audio" "ny_ic_pause_recorded_audio" "-1"
    pure NoAction
  ]

---------------------------------------------------- Chat View Model ----------------------------------------------------
eval (ChatViewActionController (ChatView.SendSuggestion optionName)) state = do
  case find (\x -> x.option == optionName) state.data.options of
    Just selectedOption -> do
      if (selectedOption.label == "CALL_DRIVER")
      then
        continue state { props { showCallDriverModel = true, isPopupModelOpen = true }, data{ selectedOption = Just selectedOption }} 
      else 
        if (selectedOption.label == "CALL_SUPPORT") 
        then
          continue state { props { showCallSupportModel = true, isPopupModelOpen = true }, data{ selectedOption = Just selectedOption } }
        else do
          let messages' = snoc state.data.chatConfig.messages (ChatView.makeChatComponent optionName "Customer" (getCurrentUTC ""))
          exit (SelectIssueOption (state { data { chatConfig { messages = messages', suggestionsList = []  }, selectedOption = Just selectedOption } }))

    Nothing -> do
      _ <- pure $ toast "Can't find option"
      continue state

eval (ChatViewActionController (ChatView.BackPressed)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
    pure $ BackPressed
  ]

eval (ChatViewActionController (ChatView.OnImageClick url)) state = do
  continue state { data  { viewImageState { image = url, imageName = Nothing } }
                 , props { showViewImageModel = true } }

eval (ChatViewActionController (ChatView.EnableSuggestions)) state =
  continue state { data { chatConfig { enableSuggestionClick = true } } }

eval (SendMessage message toggleSubmitComp) state = do
  let messages'       = snoc state.data.chatConfig.messages message
  let showSubmitComp' = if toggleSubmitComp then not state.props.showSubmitComp else state.props.showSubmitComp
  continueWithCmd state { data  { chatConfig { messages = messages' } }
                        , props { showSubmitComp = showSubmitComp' } } [do
    _ <- scrollToEnd (getNewIDWithTag "ChatScrollView") true
    pure NoAction
  ]

eval (UpdateState updatedState) state = do
  continue updatedState

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id text)) state =
  continue state { data { messageToBeSent = text } }

eval (ConfirmCall (PrimaryButton.OnClick)) state =
  case find (\x -> (state.props.showCallSupportModel && x.label == "CALL_SUPPORT") ||(state.props.showCallDriverModel && x.label == "CALL_DRIVER")) state.data.options of
    Just selectedOption -> do
      let messages' = snoc state.data.chatConfig.messages (ChatView.makeChatComponent selectedOption.option "Customer" (getCurrentUTC "") )
      if state.props.showCallDriverModel
      then
        exit $ CallDriver state { data { chatConfig { messages = messages', suggestionsList = [] }, selectedOption = Just selectedOption }, props { isPopupModelOpen = false, showCallDriverModel = false, showCallSupportModel = false } }
      else
        exit $ CallSupport state { data { chatConfig { messages = messages', suggestionsList = [] }, selectedOption = Just selectedOption }, props { isPopupModelOpen = false, showCallDriverModel = false, showCallSupportModel = false } }
    Nothing -> do
      _ <- pure $ toast "Can't find option"
      continue state

eval (CancelCall (PrimaryButton.OnClick)) state =
  continue state { props { isPopupModelOpen = false, showCallDriverModel = false, showCallSupportModel = false } }

eval _ state =
  continue state

data Result = Result Boolean