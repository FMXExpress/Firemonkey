{File generated on 28.10.2014 13:36:29 by JavaImport for Android}

unit android.speech;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.Analytics,
  Androidapi.JNI.ApkExpansion,
  Androidapi.JNI.App,
  Androidapi.JNI.Dalvik,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.InputMethodService,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Licensing,
  Androidapi.JNI.Location,
  Androidapi.JNI.Media,
  Androidapi.JNI.Net,
  Androidapi.JNI.OpenGL,
  Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Support,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Util,
  Androidapi.JNI.VideoView,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Widget;

type
  {Class forward declarations}
  JRecognitionListener = interface; {android/speech/RecognitionListener}
  JRecognitionService_Callback = interface; {android/speech/RecognitionService$Callback}
  JRecognitionService = interface; {android/speech/RecognitionService}
  JRecognizerIntent = interface; {android/speech/RecognizerIntent}
  JRecognizerResultsIntent = interface; {android/speech/RecognizerResultsIntent}
  JSpeechRecognizer = interface; {android/speech/SpeechRecognizer}

  JRecognitionListenerClass = interface(IJavaClass)
    ['{F9AC9597-8D5E-4671-ABE6-5ACFEF72DF29}']
  end;

  [JavaSignature('android/speech/RecognitionListener')]
  JRecognitionListener = interface(IJavaInstance)
    ['{BAF43C3C-8E68-4137-B6F7-7E8C5BE0F450}']
    {Methods}
    procedure onBeginningOfSpeech; cdecl;
    procedure onBufferReceived(Param0: TJavaArray<Byte>); cdecl;
    procedure onEndOfSpeech; cdecl;
    procedure onError(Param0: Integer); cdecl;
    procedure onEvent(Param0: Integer; Param1: JBundle); cdecl;
    procedure onPartialResults(Param0: JBundle); cdecl;
    procedure onReadyForSpeech(Param0: JBundle); cdecl;
    procedure onResults(Param0: JBundle); cdecl;
    procedure onRmsChanged(Param0: Single); cdecl;
  end;

  TJRecognitionListener = class(TJavaGenericImport<JRecognitionListenerClass, JRecognitionListener>)
  end;

  JRecognitionService_CallbackClass = interface(JObjectClass)
    ['{F1170301-A008-4F25-8D97-0E847318DE75}']
  end;

  [JavaSignature('android/speech/RecognitionService$Callback')]
  JRecognitionService_Callback = interface(JObject)
    ['{A92D39BD-65D0-40FC-9AC8-2350751C1652}']
    {Methods}
    procedure beginningOfSpeech; cdecl;
    procedure bufferReceived(buffer: TJavaArray<Byte>); cdecl;
    procedure endOfSpeech; cdecl;
    procedure error(error: Integer); cdecl;
    procedure partialResults(partialResults: JBundle); cdecl;
    procedure readyForSpeech(params: JBundle); cdecl;
    procedure results(results: JBundle); cdecl;
    procedure rmsChanged(rmsdB: Single); cdecl;
  end;

  TJRecognitionService_Callback = class(TJavaGenericImport<JRecognitionService_CallbackClass, JRecognitionService_Callback>)
  end;

  JRecognitionServiceClass = interface(JObjectClass)
    ['{1FF8D370-0E52-4D21-9FFC-129335749C4C}']
    {Property methods}
    function _GetSERVICE_INTERFACE: JString;
    function _GetSERVICE_META_DATA: JString;
    {Methods}
    function init: JRecognitionService; cdecl;
    {Properties}
    property SERVICE_INTERFACE: JString read _GetSERVICE_INTERFACE;
    property SERVICE_META_DATA: JString read _GetSERVICE_META_DATA;
  end;

  [JavaSignature('android/speech/RecognitionService')]
  JRecognitionService = interface(JObject)
    ['{B04DD285-70D6-4DED-9532-CD749C0298D6}']
    {Methods}
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onDestroy; cdecl;
  end;

  TJRecognitionService = class(TJavaGenericImport<JRecognitionServiceClass, JRecognitionService>)
  end;

  JRecognizerIntentClass = interface(JObjectClass)
    ['{60E03512-BE10-4451-B957-2C9127980626}']
    {Property methods}
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
    function getVoiceDetailsIntent(context: JContext): JIntent; cdecl;
    {Properties}
    property ACTION_GET_LANGUAGE_DETAILS: JString read _GetACTION_GET_LANGUAGE_DETAILS;
    property ACTION_RECOGNIZE_SPEECH: JString read _GetACTION_RECOGNIZE_SPEECH;
    property ACTION_VOICE_SEARCH_HANDS_FREE: JString read _GetACTION_VOICE_SEARCH_HANDS_FREE;
    property ACTION_WEB_SEARCH: JString read _GetACTION_WEB_SEARCH;
    property DETAILS_META_DATA: JString read _GetDETAILS_META_DATA;
    property EXTRA_CALLING_PACKAGE: JString read _GetEXTRA_CALLING_PACKAGE;
    property EXTRA_CONFIDENCE_SCORES: JString read _GetEXTRA_CONFIDENCE_SCORES;
    property EXTRA_LANGUAGE: JString read _GetEXTRA_LANGUAGE;
    property EXTRA_LANGUAGE_MODEL: JString read _GetEXTRA_LANGUAGE_MODEL;
    property EXTRA_LANGUAGE_PREFERENCE: JString read _GetEXTRA_LANGUAGE_PREFERENCE;
    property EXTRA_MAX_RESULTS: JString read _GetEXTRA_MAX_RESULTS;
    property EXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE: JString read _GetEXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE;
    property EXTRA_ORIGIN: JString read _GetEXTRA_ORIGIN;
    property EXTRA_PARTIAL_RESULTS: JString read _GetEXTRA_PARTIAL_RESULTS;
    property EXTRA_PROMPT: JString read _GetEXTRA_PROMPT;
    property EXTRA_RESULTS: JString read _GetEXTRA_RESULTS;
    property EXTRA_RESULTS_PENDINGINTENT: JString read _GetEXTRA_RESULTS_PENDINGINTENT;
    property EXTRA_RESULTS_PENDINGINTENT_BUNDLE: JString read _GetEXTRA_RESULTS_PENDINGINTENT_BUNDLE;
    property EXTRA_SECURE: JString read _GetEXTRA_SECURE;
    property EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS: JString read _GetEXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS;
    property EXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS: JString read _GetEXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS;
    property EXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS: JString read _GetEXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS;
    property EXTRA_SUPPORTED_LANGUAGES: JString read _GetEXTRA_SUPPORTED_LANGUAGES;
    property EXTRA_WEB_SEARCH_ONLY: JString read _GetEXTRA_WEB_SEARCH_ONLY;
    property LANGUAGE_MODEL_FREE_FORM: JString read _GetLANGUAGE_MODEL_FREE_FORM;
    property LANGUAGE_MODEL_WEB_SEARCH: JString read _GetLANGUAGE_MODEL_WEB_SEARCH;
    property RESULT_AUDIO_ERROR: Integer read _GetRESULT_AUDIO_ERROR;
    property RESULT_CLIENT_ERROR: Integer read _GetRESULT_CLIENT_ERROR;
    property RESULT_NETWORK_ERROR: Integer read _GetRESULT_NETWORK_ERROR;
    property RESULT_NO_MATCH: Integer read _GetRESULT_NO_MATCH;
    property RESULT_SERVER_ERROR: Integer read _GetRESULT_SERVER_ERROR;
  end;

  [JavaSignature('android/speech/RecognizerIntent')]
  JRecognizerIntent = interface(JObject)
    ['{CA17E21C-7D42-441C-AB8B-80EF6581A681}']
  end;

  TJRecognizerIntent = class(TJavaGenericImport<JRecognizerIntentClass, JRecognizerIntent>)
  end;

  JRecognizerResultsIntentClass = interface(JObjectClass)
    ['{8F0037A8-6064-4801-B0D3-B616452B2B97}']
    {Property methods}
    function _GetACTION_VOICE_SEARCH_RESULTS: JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTML: JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS: JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS: JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_STRINGS: JString;
    function _GetEXTRA_VOICE_SEARCH_RESULT_URLS: JString;
    function _GetURI_SCHEME_INLINE: JString;
    {Properties}
    property ACTION_VOICE_SEARCH_RESULTS: JString read _GetACTION_VOICE_SEARCH_RESULTS;
    property EXTRA_VOICE_SEARCH_RESULT_HTML: JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTML;
    property EXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS: JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS;
    property EXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS: JString read _GetEXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS;
    property EXTRA_VOICE_SEARCH_RESULT_STRINGS: JString read _GetEXTRA_VOICE_SEARCH_RESULT_STRINGS;
    property EXTRA_VOICE_SEARCH_RESULT_URLS: JString read _GetEXTRA_VOICE_SEARCH_RESULT_URLS;
    property URI_SCHEME_INLINE: JString read _GetURI_SCHEME_INLINE;
  end;

  [JavaSignature('android/speech/RecognizerResultsIntent')]
  JRecognizerResultsIntent = interface(JObject)
    ['{C94B7D93-1B99-4711-A07C-A3C71C70CC26}']
  end;

  TJRecognizerResultsIntent = class(TJavaGenericImport<JRecognizerResultsIntentClass, JRecognizerResultsIntent>)
  end;

  JSpeechRecognizerClass = interface(JObjectClass)
    ['{CBDC4EC6-C5C5-47E0-9C80-54B8BF670DAD}']
    {Property methods}
    function _GetCONFIDENCE_SCORES: JString;
    function _GetERROR_AUDIO: Integer;
    function _GetERROR_CLIENT: Integer;
    function _GetERROR_INSUFFICIENT_PERMISSIONS: Integer;
    function _GetERROR_NETWORK: Integer;
    function _GetERROR_NETWORK_TIMEOUT: Integer;
    function _GetERROR_NO_MATCH: Integer;
    function _GetERROR_RECOGNIZER_BUSY: Integer;
    function _GetERROR_SERVER: Integer;
    function _GetERROR_SPEECH_TIMEOUT: Integer;
    function _GetRESULTS_RECOGNITION: JString;
    {Methods}
    function createSpeechRecognizer(context: JContext): JSpeechRecognizer; cdecl; overload;
    function createSpeechRecognizer(context: JContext; serviceComponent: JComponentName): JSpeechRecognizer; cdecl; overload;
    function isRecognitionAvailable(context: JContext): Boolean; cdecl;
    {Properties}
    property CONFIDENCE_SCORES: JString read _GetCONFIDENCE_SCORES;
    property ERROR_AUDIO: Integer read _GetERROR_AUDIO;
    property ERROR_CLIENT: Integer read _GetERROR_CLIENT;
    property ERROR_INSUFFICIENT_PERMISSIONS: Integer read _GetERROR_INSUFFICIENT_PERMISSIONS;
    property ERROR_NETWORK: Integer read _GetERROR_NETWORK;
    property ERROR_NETWORK_TIMEOUT: Integer read _GetERROR_NETWORK_TIMEOUT;
    property ERROR_NO_MATCH: Integer read _GetERROR_NO_MATCH;
    property ERROR_RECOGNIZER_BUSY: Integer read _GetERROR_RECOGNIZER_BUSY;
    property ERROR_SERVER: Integer read _GetERROR_SERVER;
    property ERROR_SPEECH_TIMEOUT: Integer read _GetERROR_SPEECH_TIMEOUT;
    property RESULTS_RECOGNITION: JString read _GetRESULTS_RECOGNITION;
  end;

  [JavaSignature('android/speech/SpeechRecognizer')]
  JSpeechRecognizer = interface(JObject)
    ['{D0275AE6-E525-46F8-982C-EFDE1949D6DA}']
    {Methods}
    procedure cancel; cdecl;
    procedure destroy; cdecl;
    procedure setRecognitionListener(listener: JRecognitionListener); cdecl;
    procedure startListening(recognizerIntent: JIntent); cdecl;
    procedure stopListening; cdecl;
  end;

  TJSpeechRecognizer = class(TJavaGenericImport<JSpeechRecognizerClass, JSpeechRecognizer>)
  end;

const
  TJRecognitionService_SERVICE_INTERFACE = 'android.speech.RecognitionService';
  TJRecognitionService_SERVICE_META_DATA = 'android.speech';

  TJRecognizerIntent_ACTION_GET_LANGUAGE_DETAILS = 'android.speech.action.GET_LANGUAGE_DETAILS';
  TJRecognizerIntent_ACTION_RECOGNIZE_SPEECH = 'android.speech.action.RECOGNIZE_SPEECH';
  TJRecognizerIntent_ACTION_VOICE_SEARCH_HANDS_FREE = 'android.speech.action.VOICE_SEARCH_HANDS_FREE';
  TJRecognizerIntent_ACTION_WEB_SEARCH = 'android.speech.action.WEB_SEARCH';
  TJRecognizerIntent_DETAILS_META_DATA = 'android.speech.DETAILS';
  TJRecognizerIntent_EXTRA_CALLING_PACKAGE = 'calling_package';
  TJRecognizerIntent_EXTRA_CONFIDENCE_SCORES = 'android.speech.extra.CONFIDENCE_SCORES';
  TJRecognizerIntent_EXTRA_LANGUAGE = 'android.speech.extra.LANGUAGE';
  TJRecognizerIntent_EXTRA_LANGUAGE_MODEL = 'android.speech.extra.LANGUAGE_MODEL';
  TJRecognizerIntent_EXTRA_LANGUAGE_PREFERENCE = 'android.speech.extra.LANGUAGE_PREFERENCE';
  TJRecognizerIntent_EXTRA_MAX_RESULTS = 'android.speech.extra.MAX_RESULTS';
  TJRecognizerIntent_EXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE = 'android.speech.extra.ONLY_RETURN_LANGUAGE_PREFERENCE';
  TJRecognizerIntent_EXTRA_ORIGIN = 'android.speech.extra.ORIGIN';
  TJRecognizerIntent_EXTRA_PARTIAL_RESULTS = 'android.speech.extra.PARTIAL_RESULTS';
  TJRecognizerIntent_EXTRA_PROMPT = 'android.speech.extra.PROMPT';
  TJRecognizerIntent_EXTRA_RESULTS = 'android.speech.extra.RESULTS';
  TJRecognizerIntent_EXTRA_RESULTS_PENDINGINTENT = 'android.speech.extra.RESULTS_PENDINGINTENT';
  TJRecognizerIntent_EXTRA_RESULTS_PENDINGINTENT_BUNDLE = 'android.speech.extra.RESULTS_PENDINGINTENT_BUNDLE';
  TJRecognizerIntent_EXTRA_SECURE = 'android.speech.extras.EXTRA_SECURE';
  TJRecognizerIntent_EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS = 'android.speech.extras.SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS';
  TJRecognizerIntent_EXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS = 'android.speech.extras.SPEECH_INPUT_MINIMUM_LENGTH_MILLIS';
  TJRecognizerIntent_EXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS = 'android.speech.extras.SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_'
    + 'MILLIS';
  TJRecognizerIntent_EXTRA_SUPPORTED_LANGUAGES = 'android.speech.extra.SUPPORTED_LANGUAGES';
  TJRecognizerIntent_EXTRA_WEB_SEARCH_ONLY = 'android.speech.extra.WEB_SEARCH_ONLY';
  TJRecognizerIntent_LANGUAGE_MODEL_FREE_FORM = 'free_form';
  TJRecognizerIntent_LANGUAGE_MODEL_WEB_SEARCH = 'web_search';
  TJRecognizerIntent_RESULT_AUDIO_ERROR = 5;
  TJRecognizerIntent_RESULT_CLIENT_ERROR = 2;
  TJRecognizerIntent_RESULT_NETWORK_ERROR = 4;
  TJRecognizerIntent_RESULT_NO_MATCH = 1;
  TJRecognizerIntent_RESULT_SERVER_ERROR = 3;

  TJRecognizerResultsIntent_ACTION_VOICE_SEARCH_RESULTS = 'android.speech.action.VOICE_SEARCH_RESULTS';
  TJRecognizerResultsIntent_EXTRA_VOICE_SEARCH_RESULT_HTML = 'android.speech.extras.VOICE_SEARCH_RESULT_HTML';
  TJRecognizerResultsIntent_EXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS = 'android.speech.extras.VOICE_SEARCH_RESULT_HTML_BASE_URLS';
  TJRecognizerResultsIntent_EXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS = 'android.speech.extras.EXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS';
  TJRecognizerResultsIntent_EXTRA_VOICE_SEARCH_RESULT_STRINGS = 'android.speech.extras.VOICE_SEARCH_RESULT_STRINGS';
  TJRecognizerResultsIntent_EXTRA_VOICE_SEARCH_RESULT_URLS = 'android.speech.extras.VOICE_SEARCH_RESULT_URLS';
  TJRecognizerResultsIntent_URI_SCHEME_INLINE = 'inline';

  TJSpeechRecognizer_CONFIDENCE_SCORES = 'confidence_scores';
  TJSpeechRecognizer_ERROR_AUDIO = 3;
  TJSpeechRecognizer_ERROR_CLIENT = 5;
  TJSpeechRecognizer_ERROR_INSUFFICIENT_PERMISSIONS = 9;
  TJSpeechRecognizer_ERROR_NETWORK = 2;
  TJSpeechRecognizer_ERROR_NETWORK_TIMEOUT = 1;
  TJSpeechRecognizer_ERROR_NO_MATCH = 7;
  TJSpeechRecognizer_ERROR_RECOGNIZER_BUSY = 8;
  TJSpeechRecognizer_ERROR_SERVER = 4;
  TJSpeechRecognizer_ERROR_SPEECH_TIMEOUT = 6;
  TJSpeechRecognizer_RESULTS_RECOGNITION = 'results_recognition';

implementation

end.