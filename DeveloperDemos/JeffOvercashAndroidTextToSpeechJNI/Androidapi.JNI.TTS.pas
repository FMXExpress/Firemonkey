{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013 Jeff Overcash                       }
{                                                       }
{*******************************************************}

{ Delphi trnslation for TTS Android Java classes from                            }
{ http://developer.android.com/reference/android/speech/tts/package-summary.html }

unit Androidapi.JNI.TTS;

interface

uses Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type

  {Forward declarations}
  JSynthesisCallback = interface; // android.speech.tts.SynthesisCallback
  JSynthesisRequest = interface; // android.speech.tts.SynthesisRequest
  JTextToSpeech = interface; // android.speech.tts.TextToSpeech
  JTextToSpeech_Engine = interface; // android.speech.tts.TextToSpeech$Engine
  JTextToSpeech_EngineInfo = interface; //android.speech.tts.TextToSpeech$EngineInfo
  JTextToSpeech_OnInitListener = interface; // android.speech.tts.TextToSpeech$OnInitListener
  JTextToSpeech_OnUtteranceCompletedListener = interface; // android.speech.tts.TextToSpeech$OnUtteranceCompletedListener
  JTextToSpeechService = interface; // android.speech.tts.TextToSpeechService
  JUtteranceProgressListener = interface; // android.speech.tts.UtteranceProgressListener

  JSynthesisCallbackClass = interface(IJavaClass)
    ['{3B30766E-A43B-4005-BE92-472CD075F325}']
  end;

  [JavaSignature('android/speech/tts/SynthesisCallback')]
  JSynthesisCallback = interface(IJavaInstance)
    ['{190C13DE-BC7E-44CE-94F6-4AF1A84A4612}']
    function audioAvailable(buffer: TJavaArray<byte>; offset: Integer; length: Integer): Integer; cdecl;
    function done: Integer; cdecl;
    procedure error; cdecl;
    function getMaxBufferSize: Integer; cdecl;
    function start(sampleRateInHz: Integer; audioFormat: Integer;  channelCount: Integer): Integer; cdecl;
  end;
  TJSynthesisCallback = class(TJavaGenericImport<JSynthesisCallbackClass, JSynthesisCallback>) end;

  JSynthesisRequestClass = interface(JObjectClass)
    ['{AE41459D-42C6-4E66-B174-F6FA5216A1DA}']
    { Method }
    function init(text: JString; params: JBundle): JSynthesisRequest; cdecl;
  end;

  [JavaSignature('android/speech/tts/SynthesisRequest')]
  JSynthesisRequest = interface(JObject)
    ['{1963DAD8-C58F-4868-BF8A-B43AE7A14145}']
    function getCountry: JString; cdecl;
    function getLanguage: JString; cdecl;
    function getParams: JBundle; cdecl;
    function getPitch: Integer; cdecl;
    function getSpeechRate: Integer; cdecl;
    function getText: JString; cdecl;
    function getVariant: JString; cdecl;
  end;
  TJSynthesisRequest = class(TJavaGenericImport<JSynthesisRequestClass, JSynthesisRequest>) end;

  JTextToSpeechClass = interface(JObjectClass)
    ['{0E2C5E49-95BE-4F19-BCCD-21960D03E957}']
    { Property Methods }
    function _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString;
    function _GetERROR: Integer;
    function _GetLANG_AVAILABLE: Integer;
    function _GetLANG_COUNTRY_AVAILABLE: Integer;
    function _GetLANG_COUNTRY_VAR_AVAILABLE: Integer;
    function _GetLANG_MISSING_DATA: Integer;
    function _GetLANG_NOT_SUPPORTED: Integer;
    function _GetQUEUE_ADD: Integer;
    function _GetQUEUE_FLUSH: Integer;
    function _GetSUCCESS: Integer;
    { Methods }
    function init(contect: JContext; listener: JTextToSpeech_OnInitListener) : JTextToSpeech; cdecl; overload;
    function init(context: JContext; listener: JTextToSpeech_OnInitListener; engine: JString): JTextToSpeech; cdecl; overload;
    { Properties }
    property ACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString read _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED;
    property ERROR: Integer read _GetERROR;
    property LANG_AVAILABLE: Integer read _GetLANG_AVAILABLE;
    property LANG_COUNTRY_AVAILABLE: Integer read _GetLANG_COUNTRY_AVAILABLE;
    property LANG_COUNTRY_VAR_AVAILABLE: Integer read _GetLANG_COUNTRY_VAR_AVAILABLE;
    property LANG_MISSING_DATA: Integer read _GetLANG_MISSING_DATA;
    property LANG_NOT_SUPPORTED: Integer read _GetLANG_NOT_SUPPORTED;
    property QUEUE_ADD: Integer read _GetQUEUE_ADD;
    property QUEUE_FLUSH: Integer read _GetQUEUE_FLUSH;
    property SUCCESS: Integer read _GetSUCCESS;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech')]
  JTextToSpeech = interface(JObject)
    ['{E1D06364-F967-4381-B178-EEDD42C203C7}']
    function addEarcon(earcon: JString; filename: JString): Integer; cdecl; overload;
    function addEarcon(earcon: JString; packagename: JString; resourceID: Integer): Integer; cdecl; overload;
    function addSpeech(text: JString; filename: JString): Integer; cdecl; overload;
    function addSpeech(text: JString; packagename: JString; resourceID: Integer) : Integer; cdecl; overload;
    function areDefaultsEnforced: Boolean; cdecl;
    function getDefaultEngine: JString; cdecl;
    function getEngines: JList; cdecl;
    function getFeatures(locale: JLocale): JSet; cdecl;
    function getLanguage: JLocale; cdecl;
    function isLanguageAvailable(loc: JLocale): Integer; cdecl;
    function isSpeaking: Boolean; cdecl;
    function playEarcon(earcon: JString; queueMode: Integer; params: JHashMap) : Integer; cdecl;
    function playSilence(durationInMs: Int64; queueMode: Integer; params: JHashMap): Integer; cdecl;
    function setEngineByPackageName(enginePackageName: JString): Integer; cdecl;
    function setLanguage(loc: JLocale): Integer; cdecl;
    function setOnUtteranceCompletedListener(listener: JTextToSpeech_OnUtteranceCompletedListener): Integer; cdecl;
    function setOnUtteranceProgressListener(listener: JUtteranceProgressListener): Integer; cdecl;
    function setPitch(pitch: Single): Integer; cdecl;
    function setSpeechRate(speechRate: Single): Integer; cdecl;
    procedure shutdown; cdecl;
    function speak(text: JString; queueMode: Integer; params: JHashMap) : Integer; cdecl;
    function stop: Integer; cdecl;
    function synthesizeToFile(text: JString; params: JHashMap; filename: String) : Integer; cdecl;
  end;
  TJTextToSpeech = class(TJavaGenericImport<JTextToSpeechClass, JTextToSpeech>) end;

  JTextToSpeech_EngineClass = interface(JObjectClass)
    ['{8516EA75-A410-4EEE-8281-9ABCE1577F46}']
    { Property Methods }
    function _GeCHECK_VOICE_DATA_PASS: Integer;
    function _GetACTION_CHECK_TTS_DATA: JString;
    function _GetACTION_INSTALL_TTS_DATA: JString;
    function _GetACTION_TTS_DATA_INSTALLED: JString;
    function _GetCHECK_VOICE_DATA_BAD_DATA: Integer;
    function _GetCHECK_VOICE_DATA_FAIL: Integer;
    function _GetCHECK_VOICE_DATA_MISSING_DATA: Integer;
    function _GetCHECK_VOICE_DATA_MISSING_VOLUME: Integer;
    function _GetDEFAULT_STREAM: Integer;
    function _GetEXTRA_AVAILABLE_VOICES: JString;
    function _GetEXTRA_CHECK_VOICE_DATA_FOR: JString;
    function _GetEXTRA_TTS_DATA_INSTALLED: JString;
    function _GetEXTRA_UNAVAILABLE_VOICES: JString;
    function _GetEXTRA_VOICE_DATA_FILES: JString;
    function _GetEXTRA_VOICE_DATA_FILES_INFO: JString;
    function _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY: JString;
    function _GetINTENT_ACTION_TTS_SERVICE: JString;
    function _GetKEY_FEATURE_EMBEDDED_SYNTHESIS: JString;
    function _GetKEY_FEATURE_NETWORK_SYNTHESIS: JString;
    function _GetKEY_PARAM_PAN: JString;
    function _GetKEY_PARAM_STREAM: JString;
    function _GetKEY_PARAM_UTTERANCE_ID: JString;
    function _GetKEY_PARAM_VOLUME: JString;
    function _GetSERVICE_META_DATA: JString;
    { Methods }
    function init: JTextToSpeech_Engine; cdecl;
    { Properties }
    property ACTION_CHECK_TTS_DATA: JString read _GetACTION_CHECK_TTS_DATA;
    property ACTION_INSTALL_TTS_DATA: JString read _GetACTION_INSTALL_TTS_DATA;
    property ACTION_TTS_DATA_INSTALLED: JString read _GetACTION_TTS_DATA_INSTALLED;
    property CHECK_VOICE_DATA_BAD_DATA: Integer read _GetCHECK_VOICE_DATA_BAD_DATA;
    property CHECK_VOICE_DATA_FAIL: Integer read _GetCHECK_VOICE_DATA_FAIL;
    property CHECK_VOICE_DATA_MISSING_DATA: Integer read _GetCHECK_VOICE_DATA_MISSING_DATA;
    property CHECK_VOICE_DATA_MISSING_VOLUME: Integer read _GetCHECK_VOICE_DATA_MISSING_VOLUME;
    property CHECK_VOICE_DATA_PASS: Integer read _GeCHECK_VOICE_DATA_PASS;
    property DEFAULT_STREAM: Integer read _GetDEFAULT_STREAM;
    property EXTRA_AVAILABLE_VOICES: JString read _GetEXTRA_AVAILABLE_VOICES;
    property EXTRA_CHECK_VOICE_DATA_FOR: JString read _GetEXTRA_CHECK_VOICE_DATA_FOR;
    property EXTRA_TTS_DATA_INSTALLED: JString read _GetEXTRA_TTS_DATA_INSTALLED;
    property EXTRA_UNAVAILABLE_VOICES: JString read _GetEXTRA_UNAVAILABLE_VOICES;
    property EXTRA_VOICE_DATA_FILES: JString read _GetEXTRA_VOICE_DATA_FILES;
    property EXTRA_VOICE_DATA_FILES_INFO: JString read _GetEXTRA_VOICE_DATA_FILES_INFO;
    property EXTRA_VOICE_DATA_ROOT_DIRECTORY: JString read _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY;
    property INTENT_ACTION_TTS_SERVICE: JString read _GetINTENT_ACTION_TTS_SERVICE;
    property KEY_FEATURE_EMBEDDED_SYNTHESIS: JString read _GetKEY_FEATURE_EMBEDDED_SYNTHESIS;
    property KEY_FEATURE_NETWORK_SYNTHESIS: JString read _GetKEY_FEATURE_NETWORK_SYNTHESIS;
    property KEY_PARAM_PAN: JString read _GetKEY_PARAM_PAN;
    property KEY_PARAM_STREAM: JString read _GetKEY_PARAM_STREAM;
    property KEY_PARAM_UTTERANCE_ID: JString read _GetKEY_PARAM_UTTERANCE_ID;
    property KEY_PARAM_VOLUME: JString read _GetKEY_PARAM_VOLUME;
    property SERVICE_META_DATA: JString read _GetSERVICE_META_DATA;
  end;

  JTextToSpeech_Engine = interface(JObject)
    ['{5BAC3048-CB0C-4DC4-AF62-D0D9AE4394CF}']
  end;
  TJTextToSpeech_Engine = class(TJavaGenericImport<JTextToSpeech_EngineClass, JTextToSpeech_Engine>) end;


  JTextToSpeech_EngineInfoClass = interface(JObjectClass)
  ['{8297AD59-5A6F-4867-A185-CA09BAD90159}']
  {Methods}
    function init : JTextToSpeech_EngineInfo; cdecl;
  end;

  JTextToSpeech_EngineInfo = interface(JObject)
  ['{204B30D8-6E25-4531-847D-18588D48D9BF}']
  {property Methods}
    function _Geticon: integer;
    function _Getlabel: JString;
    function _Getname: JString;
    procedure _Seticon(const Value: integer);
    procedure _Setlabel(const Value: JString);
    procedure _Setname(const Value: JString);
  {Properties}
    property icon : integer read _Geticon write _Seticon;
    property _label : JString read _Getlabel write _Setlabel;
    property name : JString read _Getname write _Setname;
  end;
  TJTextToSpeech_EngineInfo = class(TJavaGenericImport<JTextToSpeech_EngineInfoClass, JTextToSpeech_EngineInfo>) end;

  JTextToSpeech_OnInitListenerClass = interface(IJavaClass)
    ['{58D32EFB-6528-4EC6-BA4F-28B22FE8E573}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnInitListener')]
  JTextToSpeech_OnInitListener = interface(IJavaInstance)
    ['{027DA109-680A-4A69-905D-F62E2BD1282F}']
    procedure onInit(status: Integer); cdecl;
  end;
  TJTextToSpeech_OnInitListener = class(TJavaGenericImport<JTextToSpeech_OnInitListenerClass,JTextToSpeech_OnInitListener>) end;

  JTextToSpeech_OnUtteranceCompletedListenerClass = interface(IJavaClass)
    ['{0A608BB9-A6BF-4746-8419-9317AD625DFA}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnUtteranceCompletedListener')]
  JTextToSpeech_OnUtteranceCompletedListener = interface(IJavaInstance)
    ['{00439F2B-E73C-4B93-A9E3-832AE3CC1D5F}']
    procedure onUtteranceCompleted(utteranceID: JString); cdecl;
  end;
  TJTextToSpeech_OnUtteranceCompletedListener = class(TJavaGenericImport<JTextToSpeech_OnUtteranceCompletedListenerClass, JTextToSpeech_OnUtteranceCompletedListener>) end;

  JTextToSpeechServiceClass = interface(JServiceClass)
  ['{6BCC6ADC-CBCB-4515-A5C3-E33F9787EEC2}']
    function init : JTextToSpeechService; cdecl;
  end;

  [JavaSignature('android/speech/tts/TextToSpeechService')]
  JTextToSpeechService = interface(JService)
  ['{EE3943B6-88DC-488B-9E10-E0FA9914708D}']
    function onGetFeaturesForLanguage(lang : JString; country : JString; variant : JString) : JSet; cdecl;
    function onGetLanguage : TJavaArray<JString>; cdecl;
    function onIsLanguageAvailable(lang : JString; country : JString; variant : JString) : Integer; cdecl;
    function onLoadLanguage(lang : JString; country : JString; variant : JString) : Integer; cdecl;
    procedure onStop; cdecl;
    procedure onSynthesizeText(request : JSynthesisRequest; callback : JSynthesisCallback); cdecl;
  end;
  TJTextToSpeechService = class(TJavaGenericImport<JTextToSpeechServiceClass, JTextToSpeechService>) end;

  JUtteranceProgressListenerClass = interface(JObjectClass)
    ['{0870532F-6FD7-4B1F-9DEF-B53F0095C98A}']
    { Method }
    function init: JUtteranceProgressListener; cdecl;
  end;

  [JavaSignature('android/speech/tts/UtteranceProgressListener')]
  JUtteranceProgressListener = interface(JObject)
    ['{8B03499D-0B26-4F36-90E8-F724BD78DB0C}']
    procedure onDone(utteranceID: JString); cdecl;
    procedure onError(utteranceID: JString); cdecl;
    procedure onStart(utteranceID: JString); cdecl;
  end;
  TJUtteranceProgressListener = class(TJavaGenericImport<JUtteranceProgressListenerClass, JUtteranceProgressListener>) end;

implementation

end.
