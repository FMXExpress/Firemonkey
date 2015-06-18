{File generated on 27.10.2014 21:52:56 by JavaImport for Android}

unit android.speech.tts;

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
  JSynthesisCallback = interface; {android/speech/tts/SynthesisCallback}
  JSynthesisRequest = interface; {android/speech/tts/SynthesisRequest}
  JTextToSpeech_Engine = interface; {android/speech/tts/TextToSpeech$Engine}
  JTextToSpeech_EngineInfo = interface; {android/speech/tts/TextToSpeech$EngineInfo}
  JTextToSpeech_OnInitListener = interface; {android/speech/tts/TextToSpeech$OnInitListener}
  JTextToSpeech_OnUtteranceCompletedListener = interface; {android/speech/tts/TextToSpeech$OnUtteranceCompletedListener}
  JTextToSpeech = interface; {android/speech/tts/TextToSpeech}
  JTextToSpeechService = interface; {android/speech/tts/TextToSpeechService}
  JUtteranceProgressListener = interface; {android/speech/tts/UtteranceProgressListener}
  JVoice = interface; {android/speech/tts/Voice}

  JSynthesisCallbackClass = interface(IJavaClass)
    ['{FC84BEEF-D0D9-4BD2-BBAF-17DDF014606D}']
  end;

  [JavaSignature('android/speech/tts/SynthesisCallback')]
  JSynthesisCallback = interface(IJavaInstance)
    ['{1FF42008-E6D2-4B8E-879A-1D949A8CC364}']
    {Methods}
    function audioAvailable(Param0: TJavaArray<Byte>; Param1: Integer; Param2: Integer): Integer; cdecl;
    function done: Integer; cdecl;
    procedure error; cdecl; overload;
    procedure error(Param0: Integer); cdecl; overload;
    function getMaxBufferSize: Integer; cdecl;
    function hasFinished: Boolean; cdecl;
    function hasStarted: Boolean; cdecl;
    function start(Param0: Integer; Param1: Integer; Param2: Integer): Integer; cdecl;
  end;

  TJSynthesisCallback = class(TJavaGenericImport<JSynthesisCallbackClass, JSynthesisCallback>)
  end;

  JSynthesisRequestClass = interface(JObjectClass)
    ['{EED20F43-0E0C-4746-9D31-33046472EBF6}']
    {Methods}
    function init(text: JString; params: JBundle): JSynthesisRequest; cdecl; overload;
    function init(text: JCharSequence; params: JBundle): JSynthesisRequest; cdecl; overload;
  end;

  [JavaSignature('android/speech/tts/SynthesisRequest')]
  JSynthesisRequest = interface(JObject)
    ['{7ABB19D4-C175-449F-9DF4-F5E13842C475}']
    {Methods}
    function getCallerUid: Integer; cdecl;
    function getCharSequenceText: JCharSequence; cdecl;
    function getCountry: JString; cdecl;
    function getLanguage: JString; cdecl;
    function getParams: JBundle; cdecl;
    function getPitch: Integer; cdecl;
    function getSpeechRate: Integer; cdecl;
    function getText: JString; cdecl; deprecated;
    function getVariant: JString; cdecl;
    function getVoiceName: JString; cdecl;
  end;

  TJSynthesisRequest = class(TJavaGenericImport<JSynthesisRequestClass, JSynthesisRequest>)
  end;

  JTextToSpeech_EngineClass = interface(JObjectClass)
    ['{854E0440-D0D5-4588-A90C-4E3F2DBA325E}']
    {Property methods}
    function _GetACTION_CHECK_TTS_DATA: JString;
    function _GetACTION_GET_SAMPLE_TEXT: JString;
    function _GetACTION_INSTALL_TTS_DATA: JString;
    function _GetACTION_TTS_DATA_INSTALLED: JString;
    function _GetCHECK_VOICE_DATA_BAD_DATA: Integer;
    function _GetCHECK_VOICE_DATA_FAIL: Integer;
    function _GetCHECK_VOICE_DATA_MISSING_DATA: Integer;
    function _GetCHECK_VOICE_DATA_MISSING_VOLUME: Integer;
    function _GetCHECK_VOICE_DATA_PASS: Integer;
    function _GetDEFAULT_STREAM: Integer;
    function _GetEXTRA_AVAILABLE_VOICES: JString;
    function _GetEXTRA_CHECK_VOICE_DATA_FOR: JString;
    function _GetEXTRA_SAMPLE_TEXT: JString;
    function _GetEXTRA_TTS_DATA_INSTALLED: JString;
    function _GetEXTRA_UNAVAILABLE_VOICES: JString;
    function _GetEXTRA_VOICE_DATA_FILES: JString;
    function _GetEXTRA_VOICE_DATA_FILES_INFO: JString;
    function _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY: JString;
    function _GetINTENT_ACTION_TTS_SERVICE: JString;
    function _GetKEY_FEATURE_EMBEDDED_SYNTHESIS: JString;
    function _GetKEY_FEATURE_NETWORK_RETRIES_COUNT: JString;
    function _GetKEY_FEATURE_NETWORK_SYNTHESIS: JString;
    function _GetKEY_FEATURE_NETWORK_TIMEOUT_MS: JString;
    function _GetKEY_FEATURE_NOT_INSTALLED: JString;
    function _GetKEY_PARAM_PAN: JString;
    function _GetKEY_PARAM_SESSION_ID: JString;
    function _GetKEY_PARAM_STREAM: JString;
    function _GetKEY_PARAM_UTTERANCE_ID: JString;
    function _GetKEY_PARAM_VOLUME: JString;
    function _GetSERVICE_META_DATA: JString;
    {Methods}
    function init(Param0: JTextToSpeech): JTextToSpeech_Engine; cdecl;
    {Properties}
    property ACTION_CHECK_TTS_DATA: JString read _GetACTION_CHECK_TTS_DATA;
    property ACTION_GET_SAMPLE_TEXT: JString read _GetACTION_GET_SAMPLE_TEXT;
    property ACTION_INSTALL_TTS_DATA: JString read _GetACTION_INSTALL_TTS_DATA;
    property ACTION_TTS_DATA_INSTALLED: JString read _GetACTION_TTS_DATA_INSTALLED;
    property CHECK_VOICE_DATA_BAD_DATA: Integer read _GetCHECK_VOICE_DATA_BAD_DATA;
    property CHECK_VOICE_DATA_FAIL: Integer read _GetCHECK_VOICE_DATA_FAIL;
    property CHECK_VOICE_DATA_MISSING_DATA: Integer read _GetCHECK_VOICE_DATA_MISSING_DATA;
    property CHECK_VOICE_DATA_MISSING_VOLUME: Integer read _GetCHECK_VOICE_DATA_MISSING_VOLUME;
    property CHECK_VOICE_DATA_PASS: Integer read _GetCHECK_VOICE_DATA_PASS;
    property DEFAULT_STREAM: Integer read _GetDEFAULT_STREAM;
    property EXTRA_AVAILABLE_VOICES: JString read _GetEXTRA_AVAILABLE_VOICES;
    property EXTRA_CHECK_VOICE_DATA_FOR: JString read _GetEXTRA_CHECK_VOICE_DATA_FOR;
    property EXTRA_SAMPLE_TEXT: JString read _GetEXTRA_SAMPLE_TEXT;
    property EXTRA_TTS_DATA_INSTALLED: JString read _GetEXTRA_TTS_DATA_INSTALLED;
    property EXTRA_UNAVAILABLE_VOICES: JString read _GetEXTRA_UNAVAILABLE_VOICES;
    property EXTRA_VOICE_DATA_FILES: JString read _GetEXTRA_VOICE_DATA_FILES;
    property EXTRA_VOICE_DATA_FILES_INFO: JString read _GetEXTRA_VOICE_DATA_FILES_INFO;
    property EXTRA_VOICE_DATA_ROOT_DIRECTORY: JString read _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY;
    property INTENT_ACTION_TTS_SERVICE: JString read _GetINTENT_ACTION_TTS_SERVICE;
    property KEY_FEATURE_EMBEDDED_SYNTHESIS: JString read _GetKEY_FEATURE_EMBEDDED_SYNTHESIS;
    property KEY_FEATURE_NETWORK_RETRIES_COUNT: JString read _GetKEY_FEATURE_NETWORK_RETRIES_COUNT;
    property KEY_FEATURE_NETWORK_SYNTHESIS: JString read _GetKEY_FEATURE_NETWORK_SYNTHESIS;
    property KEY_FEATURE_NETWORK_TIMEOUT_MS: JString read _GetKEY_FEATURE_NETWORK_TIMEOUT_MS;
    property KEY_FEATURE_NOT_INSTALLED: JString read _GetKEY_FEATURE_NOT_INSTALLED;
    property KEY_PARAM_PAN: JString read _GetKEY_PARAM_PAN;
    property KEY_PARAM_SESSION_ID: JString read _GetKEY_PARAM_SESSION_ID;
    property KEY_PARAM_STREAM: JString read _GetKEY_PARAM_STREAM;
    property KEY_PARAM_UTTERANCE_ID: JString read _GetKEY_PARAM_UTTERANCE_ID;
    property KEY_PARAM_VOLUME: JString read _GetKEY_PARAM_VOLUME;
    property SERVICE_META_DATA: JString read _GetSERVICE_META_DATA;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$Engine')]
  JTextToSpeech_Engine = interface(JObject)
    ['{897D3281-3726-425D-9E58-4D7F2B6D208C}']
  end;

  TJTextToSpeech_Engine = class(TJavaGenericImport<JTextToSpeech_EngineClass, JTextToSpeech_Engine>)
  end;

  JTextToSpeech_EngineInfoClass = interface(JObjectClass)
    ['{ECA5AE57-EF8E-4321-A262-58E2D3C6F13B}']
    {Methods}
    function init: JTextToSpeech_EngineInfo; cdecl;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$EngineInfo')]
  JTextToSpeech_EngineInfo = interface(JObject)
    ['{70E63150-0181-4D9F-8483-501353282A01}']
    {Property methods}
    function _Geticon: Integer;
    procedure _Seticon(Value: Integer);
    function _Getlabel: JString;
    procedure _Setlabel(Value: JString);
    function _Getname: JString;
    procedure _Setname(Value: JString);
    {Methods}
    function toString: JString; cdecl;
    {Properties}
    property icon: Integer read _Geticon write _Seticon;
    property &label: JString read _Getlabel write _Setlabel;
    property name: JString read _Getname write _Setname;
  end;

  TJTextToSpeech_EngineInfo = class(TJavaGenericImport<JTextToSpeech_EngineInfoClass, JTextToSpeech_EngineInfo>)
  end;

  JTextToSpeech_OnInitListenerClass = interface(IJavaClass)
    ['{F5ADDE50-62AC-4B6E-B9F9-C6D7F4892AA6}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnInitListener')]
  JTextToSpeech_OnInitListener = interface(IJavaInstance)
    ['{6FE4F48C-596A-4EA1-9B8E-D5040CDB9127}']
    {Methods}
    procedure onInit(Param0: Integer); cdecl;
  end;

  TJTextToSpeech_OnInitListener = class(TJavaGenericImport<JTextToSpeech_OnInitListenerClass, JTextToSpeech_OnInitListener>)
  end;

  JTextToSpeech_OnUtteranceCompletedListenerClass = interface(IJavaClass)
    ['{F111D8F0-0958-4BD1-A8E9-5966F8F9FF27}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnUtteranceCompletedListener')]
  JTextToSpeech_OnUtteranceCompletedListener = interface(IJavaInstance)
    ['{E5FF46CA-3F7D-4B6E-A661-831BC8CAFC0D}']
    {Methods}
    procedure onUtteranceCompleted(Param0: JString); cdecl;
  end;

  TJTextToSpeech_OnUtteranceCompletedListener = class(TJavaGenericImport<JTextToSpeech_OnUtteranceCompletedListenerClass, JTextToSpeech_OnUtteranceCompletedListener>)
  end;

  JTextToSpeechClass = interface(JObjectClass)
    ['{0E984D6A-3B4F-464A-97BC-470F93939C06}']
    {Property methods}
    function _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString;
    function _GetERROR: Integer;
    function _GetERROR_INVALID_REQUEST: Integer;
    function _GetERROR_NETWORK: Integer;
    function _GetERROR_NETWORK_TIMEOUT: Integer;
    function _GetERROR_NOT_INSTALLED_YET: Integer;
    function _GetERROR_OUTPUT: Integer;
    function _GetERROR_SERVICE: Integer;
    function _GetERROR_SYNTHESIS: Integer;
    function _GetLANG_AVAILABLE: Integer;
    function _GetLANG_COUNTRY_AVAILABLE: Integer;
    function _GetLANG_COUNTRY_VAR_AVAILABLE: Integer;
    function _GetLANG_MISSING_DATA: Integer;
    function _GetLANG_NOT_SUPPORTED: Integer;
    function _GetQUEUE_ADD: Integer;
    function _GetQUEUE_FLUSH: Integer;
    function _GetSTOPPED: Integer;
    function _GetSUCCESS: Integer;
    {Methods}
    function init(context: JContext; listener: JTextToSpeech_OnInitListener): JTextToSpeech; cdecl; overload;
    function init(context: JContext; listener: JTextToSpeech_OnInitListener; engine: JString): JTextToSpeech; cdecl; overload;
    function getMaxSpeechInputLength: Integer; cdecl;
    {Properties}
    property ACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString read _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED;
    property ERROR: Integer read _GetERROR;
    property ERROR_INVALID_REQUEST: Integer read _GetERROR_INVALID_REQUEST;
    property ERROR_NETWORK: Integer read _GetERROR_NETWORK;
    property ERROR_NETWORK_TIMEOUT: Integer read _GetERROR_NETWORK_TIMEOUT;
    property ERROR_NOT_INSTALLED_YET: Integer read _GetERROR_NOT_INSTALLED_YET;
    property ERROR_OUTPUT: Integer read _GetERROR_OUTPUT;
    property ERROR_SERVICE: Integer read _GetERROR_SERVICE;
    property ERROR_SYNTHESIS: Integer read _GetERROR_SYNTHESIS;
    property LANG_AVAILABLE: Integer read _GetLANG_AVAILABLE;
    property LANG_COUNTRY_AVAILABLE: Integer read _GetLANG_COUNTRY_AVAILABLE;
    property LANG_COUNTRY_VAR_AVAILABLE: Integer read _GetLANG_COUNTRY_VAR_AVAILABLE;
    property LANG_MISSING_DATA: Integer read _GetLANG_MISSING_DATA;
    property LANG_NOT_SUPPORTED: Integer read _GetLANG_NOT_SUPPORTED;
    property QUEUE_ADD: Integer read _GetQUEUE_ADD;
    property QUEUE_FLUSH: Integer read _GetQUEUE_FLUSH;
    property STOPPED: Integer read _GetSTOPPED;
    property SUCCESS: Integer read _GetSUCCESS;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech')]
  JTextToSpeech = interface(JObject)
    ['{25012B23-7607-4154-A203-6B841A7FD932}']
    {Methods}
    function addEarcon(earcon: JString; packagename: JString; resourceId: Integer): Integer; cdecl; overload;
    function addEarcon(earcon: JString; filename: JString): Integer; cdecl; overload; deprecated;
    function addEarcon(earcon: JString; &file: JFile): Integer; cdecl; overload;
    function addSpeech(text: JString; filename: JString): Integer; cdecl; overload;
    function addSpeech(text: JCharSequence; &file: JFile): Integer; cdecl; overload;
    function addSpeech(text: JString; packagename: JString; resourceId: Integer): Integer; cdecl; overload;
    function addSpeech(text: JCharSequence; packagename: JString; resourceId: Integer): Integer; cdecl; overload;
    function areDefaultsEnforced: Boolean; cdecl; deprecated;
    function getAvailableLanguages: JSet; cdecl;
    function getDefaultEngine: JString; cdecl;
    function getDefaultLanguage: JLocale; cdecl; deprecated;
    function getDefaultVoice: JVoice; cdecl;
    function getEngines: JList; cdecl;
    function getFeatures(locale: JLocale): JSet; cdecl; deprecated;
    function getLanguage: JLocale; cdecl; deprecated;
    function getVoice: JVoice; cdecl;
    function getVoices: JSet; cdecl;
    function isLanguageAvailable(loc: JLocale): Integer; cdecl;
    function isSpeaking: Boolean; cdecl;
    function playEarcon(earcon: JString; queueMode: Integer; params: JBundle; utteranceId: JString): Integer; cdecl; overload;
    function playEarcon(earcon: JString; queueMode: Integer; params: JHashMap): Integer; cdecl; overload; deprecated;
    function playSilence(durationInMs: Int64; queueMode: Integer; params: JHashMap): Integer; cdecl; deprecated;
    function playSilentUtterance(durationInMs: Int64; queueMode: Integer; utteranceId: JString): Integer; cdecl;
    function setAudioAttributes(audioAttributes: JObject {android/media/AudioAttributes}): Integer; cdecl;
    function setEngineByPackageName(enginePackageName: JString): Integer; cdecl; deprecated;
    function setLanguage(loc: JLocale): Integer; cdecl;
    function setOnUtteranceCompletedListener(listener: JTextToSpeech_OnUtteranceCompletedListener): Integer; cdecl; deprecated;
    function setOnUtteranceProgressListener(listener: JUtteranceProgressListener): Integer; cdecl;
    function setPitch(pitch: Single): Integer; cdecl;
    function setSpeechRate(speechRate: Single): Integer; cdecl;
    function setVoice(voice: JVoice): Integer; cdecl;
    procedure shutdown; cdecl;
    function speak(text: JCharSequence; queueMode: Integer; params: JBundle; utteranceId: JString): Integer; cdecl; overload;
    function speak(text: JString; queueMode: Integer; params: JHashMap): Integer; cdecl; overload; deprecated;
    function stop: Integer; cdecl;
    function synthesizeToFile(text: JCharSequence; params: JBundle; &file: JFile; utteranceId: JString): Integer; cdecl; overload;
    function synthesizeToFile(text: JString; params: JHashMap; filename: JString): Integer; cdecl; overload; deprecated;
  end;

  TJTextToSpeech = class(TJavaGenericImport<JTextToSpeechClass, JTextToSpeech>)
  end;

  JTextToSpeechServiceClass = interface(JObjectClass)
    ['{D46768C4-5AA7-44CB-BFE7-2AB2A3784CE9}']
    {Methods}
    function init: JTextToSpeechService; cdecl;
  end;

  [JavaSignature('android/speech/tts/TextToSpeechService')]
  JTextToSpeechService = interface(JObject)
    ['{524B6E55-C455-4D82-80A0-6658F0A0717B}']
    {Methods}
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onCreate; cdecl;
    procedure onDestroy; cdecl;
    function onGetDefaultVoiceNameFor(lang: JString; country: JString; variant: JString): JString; cdecl;
    function onGetVoices: JList; cdecl;
    function onIsValidVoiceName(voiceName: JString): Integer; cdecl;
    function onLoadVoice(voiceName: JString): Integer; cdecl;
  end;

  TJTextToSpeechService = class(TJavaGenericImport<JTextToSpeechServiceClass, JTextToSpeechService>)
  end;

  JUtteranceProgressListenerClass = interface(JObjectClass)
    ['{F5E8498E-A626-4960-8160-DDB9D8C83B3A}']
    {Methods}
    function init: JUtteranceProgressListener; cdecl;
  end;

  [JavaSignature('android/speech/tts/UtteranceProgressListener')]
  JUtteranceProgressListener = interface(JObject)
    ['{DE627F17-EB6E-4C8C-8B85-CFF96B5D8AE3}']
    {Methods}
    procedure onDone(Param0: JString); cdecl;
    procedure onError(Param0: JString); cdecl; overload; deprecated;
    procedure onError(utteranceId: JString; errorCode: Integer); cdecl; overload;
    procedure onStart(Param0: JString); cdecl;
  end;

  TJUtteranceProgressListener = class(TJavaGenericImport<JUtteranceProgressListenerClass, JUtteranceProgressListener>)
  end;

  JVoiceClass = interface(JObjectClass)
    ['{CC2C0BEA-0649-47FA-AA54-05F941FB158F}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    function _GetLATENCY_HIGH: Integer;
    function _GetLATENCY_LOW: Integer;
    function _GetLATENCY_NORMAL: Integer;
    function _GetLATENCY_VERY_HIGH: Integer;
    function _GetLATENCY_VERY_LOW: Integer;
    function _GetQUALITY_HIGH: Integer;
    function _GetQUALITY_LOW: Integer;
    function _GetQUALITY_NORMAL: Integer;
    function _GetQUALITY_VERY_HIGH: Integer;
    function _GetQUALITY_VERY_LOW: Integer;
    {Methods}
    function init(name: JString; locale: JLocale; quality: Integer; latency: Integer; requiresNetworkConnection: Boolean; features: JSet): JVoice; cdecl;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
    property LATENCY_HIGH: Integer read _GetLATENCY_HIGH;
    property LATENCY_LOW: Integer read _GetLATENCY_LOW;
    property LATENCY_NORMAL: Integer read _GetLATENCY_NORMAL;
    property LATENCY_VERY_HIGH: Integer read _GetLATENCY_VERY_HIGH;
    property LATENCY_VERY_LOW: Integer read _GetLATENCY_VERY_LOW;
    property QUALITY_HIGH: Integer read _GetQUALITY_HIGH;
    property QUALITY_LOW: Integer read _GetQUALITY_LOW;
    property QUALITY_NORMAL: Integer read _GetQUALITY_NORMAL;
    property QUALITY_VERY_HIGH: Integer read _GetQUALITY_VERY_HIGH;
    property QUALITY_VERY_LOW: Integer read _GetQUALITY_VERY_LOW;
  end;

  [JavaSignature('android/speech/tts/Voice')]
  JVoice = interface(JObject)
    ['{16A6AB6E-7EE1-440B-8FB7-3FCDE0216E75}']
    {Methods}
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getFeatures: JSet; cdecl;
    function getLatency: Integer; cdecl;
    function getLocale: JLocale; cdecl;
    function getName: JString; cdecl;
    function getQuality: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isNetworkConnectionRequired: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJVoice = class(TJavaGenericImport<JVoiceClass, JVoice>)
  end;

const
  TJTextToSpeech_Engine_ACTION_CHECK_TTS_DATA = 'android.speech.tts.engine.CHECK_TTS_DATA';
  TJTextToSpeech_Engine_ACTION_GET_SAMPLE_TEXT = 'android.speech.tts.engine.GET_SAMPLE_TEXT';
  TJTextToSpeech_Engine_ACTION_INSTALL_TTS_DATA = 'android.speech.tts.engine.INSTALL_TTS_DATA';
  TJTextToSpeech_Engine_ACTION_TTS_DATA_INSTALLED = 'android.speech.tts.engine.TTS_DATA_INSTALLED';
  TJTextToSpeech_Engine_CHECK_VOICE_DATA_BAD_DATA = -1;
  TJTextToSpeech_Engine_CHECK_VOICE_DATA_FAIL = 0;
  TJTextToSpeech_Engine_CHECK_VOICE_DATA_MISSING_DATA = -2;
  TJTextToSpeech_Engine_CHECK_VOICE_DATA_MISSING_VOLUME = -3;
  TJTextToSpeech_Engine_CHECK_VOICE_DATA_PASS = 1;
  TJTextToSpeech_Engine_DEFAULT_STREAM = 3;
  TJTextToSpeech_Engine_EXTRA_AVAILABLE_VOICES = 'availableVoices';
  TJTextToSpeech_Engine_EXTRA_CHECK_VOICE_DATA_FOR = 'checkVoiceDataFor';
  TJTextToSpeech_Engine_EXTRA_SAMPLE_TEXT = 'sampleText';
  TJTextToSpeech_Engine_EXTRA_TTS_DATA_INSTALLED = 'dataInstalled';
  TJTextToSpeech_Engine_EXTRA_UNAVAILABLE_VOICES = 'unavailableVoices';
  TJTextToSpeech_Engine_EXTRA_VOICE_DATA_FILES = 'dataFiles';
  TJTextToSpeech_Engine_EXTRA_VOICE_DATA_FILES_INFO = 'dataFilesInfo';
  TJTextToSpeech_Engine_EXTRA_VOICE_DATA_ROOT_DIRECTORY = 'dataRoot';
  TJTextToSpeech_Engine_INTENT_ACTION_TTS_SERVICE = 'android.intent.action.TTS_SERVICE';
  TJTextToSpeech_Engine_KEY_FEATURE_EMBEDDED_SYNTHESIS = 'embeddedTts';
  TJTextToSpeech_Engine_KEY_FEATURE_NETWORK_RETRIES_COUNT = 'networkRetriesCount';
  TJTextToSpeech_Engine_KEY_FEATURE_NETWORK_SYNTHESIS = 'networkTts';
  TJTextToSpeech_Engine_KEY_FEATURE_NETWORK_TIMEOUT_MS = 'networkTimeoutMs';
  TJTextToSpeech_Engine_KEY_FEATURE_NOT_INSTALLED = 'notInstalled';
  TJTextToSpeech_Engine_KEY_PARAM_PAN = 'pan';
  TJTextToSpeech_Engine_KEY_PARAM_SESSION_ID = 'sessionId';
  TJTextToSpeech_Engine_KEY_PARAM_STREAM = 'streamType';
  TJTextToSpeech_Engine_KEY_PARAM_UTTERANCE_ID = 'utteranceId';
  TJTextToSpeech_Engine_KEY_PARAM_VOLUME = 'volume';
  TJTextToSpeech_Engine_SERVICE_META_DATA = 'android.speech.tts';

  TJTextToSpeech_ACTION_TTS_QUEUE_PROCESSING_COMPLETED = 'android.speech.tts.TTS_QUEUE_PROCESSING_COMPLETED';
  TJTextToSpeech_ERROR = -1;
  TJTextToSpeech_ERROR_INVALID_REQUEST = -8;
  TJTextToSpeech_ERROR_NETWORK = -6;
  TJTextToSpeech_ERROR_NETWORK_TIMEOUT = -7;
  TJTextToSpeech_ERROR_NOT_INSTALLED_YET = -9;
  TJTextToSpeech_ERROR_OUTPUT = -5;
  TJTextToSpeech_ERROR_SERVICE = -4;
  TJTextToSpeech_ERROR_SYNTHESIS = -3;
  TJTextToSpeech_LANG_AVAILABLE = 0;
  TJTextToSpeech_LANG_COUNTRY_AVAILABLE = 1;
  TJTextToSpeech_LANG_COUNTRY_VAR_AVAILABLE = 2;
  TJTextToSpeech_LANG_MISSING_DATA = -1;
  TJTextToSpeech_LANG_NOT_SUPPORTED = -2;
  TJTextToSpeech_QUEUE_ADD = 1;
  TJTextToSpeech_QUEUE_FLUSH = 0;
  TJTextToSpeech_STOPPED = -2;
  TJTextToSpeech_SUCCESS = 0;

  TJVoice_LATENCY_HIGH = 400;
  TJVoice_LATENCY_LOW = 200;
  TJVoice_LATENCY_NORMAL = 300;
  TJVoice_LATENCY_VERY_HIGH = 500;
  TJVoice_LATENCY_VERY_LOW = 100;
  TJVoice_QUALITY_HIGH = 400;
  TJVoice_QUALITY_LOW = 200;
  TJVoice_QUALITY_NORMAL = 300;
  TJVoice_QUALITY_VERY_HIGH = 500;
  TJVoice_QUALITY_VERY_LOW = 100;

implementation

end.