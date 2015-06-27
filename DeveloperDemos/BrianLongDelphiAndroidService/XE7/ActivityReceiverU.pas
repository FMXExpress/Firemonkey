unit ActivityReceiverU;

interface

uses
  FMX.Types,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText;

type
  JActivityReceiverClass = interface(JBroadcastReceiverClass)
  ['{9D967671-9CD8-483A-98C8-161071CE7B64}']
    {Methods}
  //  function init: JActivityReceiver; cdecl;
  end;

  [JavaSignature('com/blong/test/ActivityReceiver')]
  JActivityReceiver = interface(JBroadcastReceiver)
  ['{4B30D537-5221-4451-893D-7916ED11CE1F}']
    {Methods}
  end;

  TJActivityReceiver = class(TJavaGenericImport<JActivityReceiverClass, JActivityReceiver>)
  protected
    constructor _Create;
  public
    class function Create: JActivityReceiver;
    procedure OnReceive(Context: JContext; ReceivedIntent: JIntent);
  end;

implementation

uses
  MainFormU,
  System.Classes,
  System.SysUtils,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  Androidapi.NativeActivity,
  Androidapi.JNI,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Toast, ServiceU;

{$REGION 'JNI setup code and callback'}
var
  ActivityReceiver: TJActivityReceiver;

//This is called from the Java activity's onReceiveNative() method
procedure ActivityReceiverOnReceiveNative(PEnv: PJNIEnv; This: JNIObject; JNIContext, JNIReceivedIntent: JNIObject); cdecl;
begin
  Log.d('+ActivityReceiverOnReceiveNative');
  Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId]);
  TThread.Queue(nil,
    procedure
    begin
      Log.d('+ThreadSwitcher');
      Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)', [MainThreadID, TThread.CurrentThread.ThreadID,
        TJThread.JavaClass.CurrentThread.getId]);
      ActivityReceiver.OnReceive(TJContext.Wrap(JNIContext), TJIntent.Wrap(JNIReceivedIntent));
      Log.d('-ThreadSwitcher');
    end);
  Log.d('-ActivityReceiverOnReceiveNative');
end;

procedure RegisterDelphiNativeMethods;
var
  PEnv: PJNIEnv;
  ReceiverClass: JNIClass;
  NativeMethod: JNINativeMethod;
begin
  Log.d('Starting the Activity Receiver JNI stuff');

  PEnv := TJNIResolver.GetJNIEnv;

  Log.d('Registering interop methods');

  NativeMethod.Name := 'activityReceiverOnReceiveNative';
  NativeMethod.Signature := '(Landroid/content/Context;Landroid/content/Intent;)V';
  NativeMethod.FnPtr := @ActivityReceiverOnReceiveNative;

  ReceiverClass := TJNIResolver.GetJavaClassID('com.blong.test.ActivityReceiver');

  PEnv^.RegisterNatives(PEnv, ReceiverClass, @NativeMethod, 1);

  PEnv^.DeleteLocalRef(PEnv, ReceiverClass);
end;
{$ENDREGION}

{ TActivityReceiver }

constructor TJActivityReceiver._Create;
begin
  inherited;
  Log.d('TJActivityReceiver._Create constructor');
end;

class function TJActivityReceiver.Create: JActivityReceiver;
begin
  Log.d('TJActivityReceiver.Create class function');
  Result := inherited Create;
  ActivityReceiver := TJActivityReceiver._Create;
end;

procedure TJActivityReceiver.OnReceive(Context: JContext;
  ReceivedIntent: JIntent);
var
  Iteration: Integer;
  CalculatedValue: Integer;
begin
  //In this sample the activity receiver is unregistered when the app goes to
  //the background. So this code won't run if the app isn't active. That
  //includes the Toast, which won't display.
  //However the service thread will continue to do its thing in the background.
  Log.d('Activity receiver received a message');
  Iteration := ReceivedIntent.getIntExtra(StringToJString(TSampleService.ID_INT_ITERATION), 0);
  CalculatedValue := ReceivedIntent.getIntExtra(StringToJString(TSampleService.ID_INT_CALCULATED_VALUE), 0);
  Toast(Format(
    'Received call %d from service'#10'with calculated value %d',
    [Iteration, CalculatedValue]),
    TToastLength.LongToast)
end;

initialization
  RegisterDelphiNativeMethods
end.
