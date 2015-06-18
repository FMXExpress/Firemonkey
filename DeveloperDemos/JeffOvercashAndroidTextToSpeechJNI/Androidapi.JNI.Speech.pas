unit Androidapi.JNI.Speech;

interface

uses Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type
  {Forward declarations}
  JRecognitionListener = interface; //android.speech.RecognitionListener
  JRecognitionService = interface; // android.speech.RecognitionService
  JRecognitionService_Callback = interface;// android.speech.RecognitionService$Callback
  JRecognizerIntent =interface; // android.speech.RecognizerIntent
  JRecognizerResultsIntent = interface; // android.speech.RecognizerResultsIntent
  JSpeechRecognizer = interface; // android/speech/SpeechRecognizer

  JRecognitionListenerClass = interface(IJavaClass)
  ['{BCCBB462-3115-4C64-9F0D-49E748CA3FB1}']
  end;

  [JavaSignature('android/speech/RecognitionListener')]
  JRecognitionListener = interface(IJavaInstance)
  ['{F7BF00EA-7876-42F1-AC4E-1F4DABE97010}']
    procedure onBeginningOfSpeech; cdecl;
    procedure onBufferReceived(buffer: TJavaArray<byte>); cdecl;
    procedure onEndOfSpeech; cdecl;
    procedure onError(error: Integer); cdecl;
    procedure onEvent(eventType : Integer; params : JBundle); cdecl;
    procedure onPartialResults(partialResults : JBundle); cdecl;
    procedure onReadyForSpeech(params : JBundle); cdecl;
    procedure onResults(results : JBundle); cdecl;
    procedure onRmsChanged(rmsdB : Single); cdecl;
  end;
  TJRecognitionListener = class(TJavaGenericImport<JRecognitionListenerClass, JRecognitionListener>) end;

  JRecognitionServiceClass = interface(JServiceClass)
  ['{F66F1C2B-A1D8-478F-96F8-49F0D6EB752C}']
  {Property Methods}
    function _GetSERVICE_INTERFACE : JString;
    function _GetSERVICE_META_DATA : JString;
  {Methods}
    function init : JRecognitionListener; cdecl;
  {Properties}
    property SERVICE_INTERFACE : JString read _GetSERVICE_INTERFACE;
    property SERVICE_META_DATA : JString read _GetSERVICE_META_DATA;
  end;

  [JavaSignature('android/speech/RecognitionService')]
  JRecognitionService = interface(JService)
  ['{8385C1E9-3E2A-481B-8140-AA1E262F24C8}']
    function onBind(intent : JIntent) : JBinder; cdecl;
    procedure onDestroy; cdecl;
    procedure onCancel(listener : JRecognitionService_Callback); cdecl;
    procedure onStartListening(intent : JIntent; listener : JRecognitionService_Callback); cdecl;
    procedure onStopListening(listener : JRecognitionService_Callback); cdecl;
  end;
  TJRecognitionService = class(TJavaGenericImport<JRecognitionServiceClass, JRecognitionService>) end;

  JRecognitionService_CallbackClass = interface(JObjectClass)
  ['{19665500-5532-4846-B585-A569A5A597D1}']
    function init : JRecognitionService_Callback;  cdecl;
  end;

  [JavaSignature('android/speech/RecognitionService$Callback')]
  JRecognitionService_Callback = interface(JObject)
  ['{2C77050F-52B6-4D5E-9E05-CC003C1D5C08}']
    procedure beginningOfSpeech;
    procedure bufferReceived(buffer: TJavaArray<byte>); cdecl;
    procedure endOfSpeech; cdecl;
    procedure error(error : Integer); cdecl;
    procedure partialResults(partialResults : JBundle); cdecl;
    procedure readyForSpeech(params : JBundle); cdecl;
    procedure results(results : JBundle); cdecl;
    procedure rmsChanged(rmsdB : Single); cdecl;
  end;
  TJRecognitionService_Callback = class(TJavaGenericImport<JRecognitionService_CallbackClass, JRecognitionService_Callback>) end;

  JRecognizerIntentClass = interface(JObjectClass)
  ['{FE988404-2BAC-4927-9BF9-4E854E43183C}']
  {Property Methods}
    function _GetACTION_GET_LANGUAGE_DETAILS: JString;
    function _GetACTION_RECOGNIZE_SPEECH: JString;
    function _GetACTION_VOICE_SEARCH_HANDS_FREE: JString;
    function _GetACTION_WEB_SEARCH: JString;
    function _GetDETAILS_META_DATA: JString;
    function _GetEXTRA_CALLING_PACKAGE: JString;
    function _GetEXTRA_CONFIDENCE_SCORES: JString;
    function _GetEXTRA_LANGUAGE: JString;
    function _GetEXTRA_LANGUAGE_MODEL: JString;
    function _GetEXTRA_LANGUAGE_PREFERENCE: JString;
    function _GetEXTRA_MAX_RESULTS: JString;
    function _GetEXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE: JString;
    function _GetEXTRA_ORIGIN: JString;
    function _GetEXTRA_PARTIAL_RESULTS: JString;
    function _GetEXTRA_PROMPT: JString;
    function _GetEXTRA_RESULTS: JString;
    function _GetEXTRA_RESULTS_PENDINGINTENT: JString;
    function _GetEXTRA_RESULTS_PENDINGINTENT_BUNDLE: JString;
    function _GetEXTRA_SECURE: JString;
    function _GetEXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS: JString;
    function _GetEXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS: JString;
    function _GetEXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS: JString;
    function _GetEXTRA_SUPPORTED_LANGUAGES: JString;
    function _GetEXTRA_WEB_SEARCH_ONLY: JString;
    function _GetLANGUAGE_MODEL_FREE_FORM: JString;
    function _GetLANGUAGE_MODEL_WEB_SEARCH: JString;
    function _GetRESULT_AUDIO_ERROR: Integer;
    function _GetRESULT_CLIENT_ERROR: Integer;
    function _GetRESULT_NETWORK_ERROR: Integer;
    function _GetRESULT_NO_MATCH: Integer;
    function _GetRESULT_SERVER_ERROR: Integer;
  {Methods}
    function init : JRecognizerIntent; cdecl;
    function getVoiceDetailsIntent(context : JContext) : JIntent; cdecl;
  {Properties}
    property ACTION_GET_LANGUAGE_DETAILS : JString read _GetACTION_GET_LANGUAGE_DETAILS;
    property ACTION_RECOGNIZE_SPEECH : JString read _GetACTION_RECOGNIZE_SPEECH;
    property ACTION_VOICE_SEARCH_HANDS_FREE : JString read _GetACTION_VOICE_SEARCH_HANDS_FREE;
    property ACTION_WEB_SEARCH : JString read _GetACTION_WEB_SEARCH;
    property DETAILS_META_DATA : JString read _GetDETAILS_META_DATA;
    property EXTRA_CALLING_PACKAGE : JString read _GetEXTRA_CALLING_PACKAGE;
    property EXTRA_CONFIDENCE_SCORES : JString read _GetEXTRA_CONFIDENCE_SCORES;
    property EXTRA_LANGUAGE : JString read _GetEXTRA_LANGUAGE;
    property EXTRA_LANGUAGE_MODEL : JString read _GetEXTRA_LANGUAGE_MODEL;
    property EXTRA_LANGUAGE_PREFERENCE : JString read _GetEXTRA_LANGUAGE_PREFERENCE;
    property EXTRA_MAX_RESULTS : JString read _GetEXTRA_MAX_RESULTS;
    property EXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE : JString read _GetEXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE;
    property EXTRA_ORIGIN : JString read _GetEXTRA_ORIGIN;
    property EXTRA_PARTIAL_RESULTS : JString read _GetEXTRA_PARTIAL_RESULTS;
    property EXTRA_PROMPT : JString read _GetEXTRA_PROMPT;
    property EXTRA_RESULTS : JString read _GetEXTRA_RESULTS;
    property EXTRA_RESULTS_PENDINGINTENT : JString read _GetEXTRA_RESULTS_PENDINGINTENT;
    property EXTRA_RESULTS_PENDINGINTENT_BUNDLE : JString read _GetEXTRA_RESULTS_PENDINGINTENT_BUNDLE;
    property EXTRA_SECURE : JString read _GetEXTRA_SECURE;
    property EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS : JString read _GetEXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS;
    property EXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS : JString read _GetEXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS;
    property EXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS : JString read _GetEXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS;
    property EXTRA_SUPPORTED_LANGUAGES : JString read _GetEXTRA_SUPPORTED_LANGUAGES;
    property EXTRA_WEB_SEARCH_ONLY : JString read _GetEXTRA_WEB_SEARCH_ONLY;
    property LANGUAGE_MODEL_FREE_FORM : JString read _GetLANGUAGE_MODEL_FREE_FORM;
    property LANGUAGE_MODEL_WEB_SEARCH : JString read _GetLANGUAGE_MODEL_WEB_SEARCH;
    property RESULT_AUDIO_ERROR : Integer read _GetRESULT_AUDIO_ERROR;
    property RESULT_CLIENT_ERROR : Integer read _GetRESULT_CLIENT_ERROR;
    property RESULT_NETWORK_ERROR : Integer read _GetRESULT_NETWORK_ERROR;
    property RESULT_NO_MATCH : Integer read _GetRESULT_NO_MATCH;
    property RESULT_SERVER_ERROR : Integer read _GetRESULT_SERVER_ERROR;
  end;

  [JavaSignature('android/speech/RecognizerIntent')]
  JRecognizerIntent = interface(JObject)
  ['{8DBAFF97-5238-4133-9A6C-1151422A3C87}']
  end;
  TJRecognizerIntent = class(TJavaGenericImport<JRecognizerIntentClass, JRecognizerIntent>) end;

  JRecognizerResultsIntentClass = interface(JObjectClass)
  ['{4D0D0DEB-AC55-46A8-9E52-51EF10AD818D}']
  {Property Methods}
    function _GetACTION_VOICE_SEARCH_RESULTS : JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTML : JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS : JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS : JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_STRINGS : JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_URLS : JString;
    function _GetURI_SCHEME_INLINE : JString;
  {Methods}
    function init : JRecognizerResultsIntent; cdecl;
  {Properties}
    property ACTION_VOICE_SEARCH_RESULTS : JString read _GetACTION_VOICE_SEARCH_RESULTS;
    property EXTRA_VOICE_SEARCH_RESULT_HTML : JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTML;
    property EXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS : JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS;
    property EXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS : JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS;
    property EXTRA_VOICE_SEARCH_RESULT_STRINGS : JString read _GetEXTRA_VOICE_SEARCH_RESULT_STRINGS;
    property EXTRA_VOICE_SEARCH_RESULT_URLS : JString read _GetEXTRA_VOICE_SEARCH_RESULT_URLS;
    property URI_SCHEME_INLINE : JString read _GetURI_SCHEME_INLINE;
  end;

  [JavaSignature('android/speech/RecognizerResultsIntent')]
  JRecognizerResultsIntent = interface(JObject)
  ['{9F471804-368D-4A6D-8F13-C6E3FC67D5FC}']
  end;
  TJRecognizerResultsIntent = class(TJavaGenericImport<JRecognizerResultsIntentClass, JRecognizerResultsIntent>) end;

  JSpeechRecognizerClass = interface(JObjectClass)
  ['{EB868DFA-9AA4-4DE0-8951-9D41816A1696}']
  {Property Methods}
    function _GetCONFIDENCE_SCORES : JString;
    function _GetERROR_AUDIO : Integer;
    function _GetERROR_CLIENT : Integer;
    function _GetERROR_INSUFFICIENT_PERMISSIONS : Integer;
    function _GetERROR_NETWORK : Integer;
    function _GetERROR_NETWORK_TIMEOUT : Integer;
    function _GetERROR_NO_MATCH : Integer;
    function _GetERROR_RECOGNIZER_BUSY : Integer;
    function _GetERROR_SERVER : Integer;
    function _GetERROR_SPEECH_TIMEOUT : Integer;
    function _GetRESULTS_RECOGNITION : JString;
  {Methods}
    function init : JSpeechRecognizer; cdecl;
    function createSpeechRecognizer(context : JContext) : JSpeechRecognizer; overload; cdecl;
    function createSpeechRecognizer(context : JContext; serviceComponent : JComponentName) : JSpeechRecognizer; overload; cdecl;
    function isRecognitionAvailable(context : JContext) : Boolean; cdecl;
  {Properties}
    property CONFIDENCE_SCORES : JString read _GetCONFIDENCE_SCORES;
    property ERROR_AUDIO : Integer read _GetERROR_AUDIO;
    property ERROR_CLIENT : Integer read _GetERROR_CLIENT;
    property ERROR_INSUFFICIENT_PERMISSIONS : Integer read _GetERROR_INSUFFICIENT_PERMISSIONS;
    property ERROR_NETWORK : Integer read _GetERROR_NETWORK;
    property ERROR_NETWORK_TIMEOUT : Integer read _GetERROR_NETWORK_TIMEOUT;
    property ERROR_NO_MATCH : Integer read _GetERROR_NO_MATCH;
    property ERROR_RECOGNIZER_BUSY : Integer read _GetERROR_RECOGNIZER_BUSY;
    property ERROR_SERVER : Integer read _GetERROR_SERVER;
    property ERROR_SPEECH_TIMEOUT : Integer read _GetERROR_SPEECH_TIMEOUT;
    property RESULTS_RECOGNITION : JString read _GetRESULTS_RECOGNITION;
  end;

  [JavaSignature('android/speech/SpeechRecognizer')]
  JSpeechRecognizer = interface(JObject)
  ['{04AFA0E1-BFF3-4270-B38D-7D380B98662A}']
    procedure cancel; cdecl;
    procedure destroy; cdecl;
    procedure setRecognitionListener(listener : JRecognitionListener); cdecl;
    procedure startListening(recognizerIntent : JIntent); cdecl;
    procedure stopListening; cdecl;
  end;
  TJSpeechRecognizer = class(TJavaGenericImport<JSpeechRecognizerClass, JSpeechRecognizer>) end;

implementation

end.
