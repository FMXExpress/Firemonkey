unit Android.ProgressDialog;

interface

uses
  AndroidAPI.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
  AndroidAPI.JNI.JavaTypes;

type
  JProgressDialog = interface;

  JProgressDialogClass = interface (JAlertDialogClass)
    ['{4A287A8D-51CF-45CA-A8A6-D5D2F8B78B9E}']
    {Property Methods}
    function _GetSTYLE_HORIZONTAL: Integer;
    function _GetSTYLE_SPINNER: Integer;
    {Properties}
    property STYLE_HORIZONTAL: Integer read _GetSTYLE_HORIZONTAL;
    property STYLE_SPINNER: Integer read _GetSTYLE_SPINNER;

    function show(context: JContext; title: JCharSequence; message: JCharSequence): JProgressDialog; cdecl; overload;
    function show(context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: Boolean; cancelable: Boolean): JProgressDialog; cdecl; overload;
    function show(context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: Boolean; cancelable: Boolean; cancelListener: JDialogInterface_OnCancelListener): JProgressDialog; cdecl; overload;
    function show(context: JContext; title: JCharSequence; message: JCharSequence; indeterminate: Boolean): JProgressDialog; cdecl; overload;
  end;

  [JavaSignature('android/app/ProgressDialog')]
  JProgressDialog = interface(JAlertDialog)
    ['{919C1A6E-1754-4E5D-8BA7-D6659C7FE88D}']
    function getMax: Integer; cdecl;
    function getProgress: Integer; cdecl;
    function getSecondaryProgress: Integer; cdecl;
    procedure incrementProgressBy(diff: Integer); cdecl;
    function isIndeterminate: Boolean; cdecl;
    procedure onStart; cdecl;
    procedure setIndeterminate(indeterminate: Boolean); cdecl;
    procedure setIndeterminateDrawable(d: JDrawable); cdecl;
    procedure setMax(max: Integer); cdecl;
    procedure setMessage(message: JCharSequence); cdecl;
    procedure setProgress(value: Integer); cdecl;
    procedure setProgressDrawable(d: JDrawable); cdecl;
    procedure setProgressNumberFormat(format: JString); cdecl;
    //procedure setProgressPercentFormat(format: JNumberFormat); cdecl;
    procedure setProgressStyle(style: Integer);
    procedure setSecondaryProgress(secondaryProgress: Integer);
  end;
  TJProgressDialog = class(TJavaGenericImport<JProgressDialogClass, JProgressDialog>)
  end;

implementation

end.

