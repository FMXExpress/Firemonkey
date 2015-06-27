unit ServiceReceiverU;

interface

uses
  FMX.Types,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  ServiceU;

type
  JServiceReceiverClass = interface(JBroadcastReceiverClass)
  ['{177320F8-F933-4E10-8106-7A5119603383}']
    {Methods}
  //  function init: JServiceReceiver; cdecl;
  end;

  [JavaSignature('com/blong/test/ServiceReceiver')]
  JServiceReceiver = interface(JBroadcastReceiver)
  ['{A1F1C3CF-7C02-4B8F-8AB2-E4B552286B35}']
    {Methods}
  end;

  TJServiceReceiver = class(TJavaGenericImport<JServiceReceiverClass, JServiceReceiver>)
  private
    [weak]FOwningService: TSampleService;
  protected
    constructor _Create(Service: TSampleService);
  public
    class function Create(Service: TSampleService): JServiceReceiver;
    procedure OnReceive(Context: JContext; ReceivedIntent: JIntent);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  FMX.Helpers.Android,
  Androidapi.NativeActivity,
  Androidapi.JNI,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Toast;

{$REGION 'JNI setup code and callback'}
var
  ServiceReceiver: TJServiceReceiver;

//This is called from the Java activity's onReceiveNative() method
procedure ServiceReceiverOnReceiveNative(PEnv: PJNIEnv; This: JNIObject; JNIContext, JNIReceivedIntent: JNIObject); cdecl;
begin
  Log.d('+ServiceReceiverOnReceiveNative');
  Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId]);
  TThread.Queue(nil,
    procedure
    begin
      Log.d('+ThreadSwitcher');
      Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)', [MainThreadID, TThread.CurrentThread.ThreadID,
        TJThread.JavaClass.CurrentThread.getId]);
      ServiceReceiver.OnReceive(TJContext.Wrap(JNIContext), TJIntent.Wrap(JNIReceivedIntent));
      Log.d('-ThreadSwitcher');
    end);
  Log.d('-ServiceReceiverOnReceiveNative');
end;

procedure RegisterDelphiNativeMethods;
var
  PEnv: PJNIEnv;
  ReceiverClass: JNIClass;
  NativeMethod: JNINativeMethod;
begin
  Log.d('Starting the Service Receiver JNI stuff');

  PEnv := TJNIResolver.GetJNIEnv;

  Log.d('Registering interop methods');

  NativeMethod.Name := 'serviceReceiverOnReceiveNative';
  NativeMethod.Signature := '(Landroid/content/Context;Landroid/content/Intent;)V';
  NativeMethod.FnPtr := @ServiceReceiverOnReceiveNative;

  ReceiverClass := TJNIResolver.GetJavaClassID('com.blong.test.ServiceReceiver');

  PEnv^.RegisterNatives(PEnv, ReceiverClass, @NativeMethod, 1);

  PEnv^.DeleteLocalRef(PEnv, ReceiverClass);
end;
{$ENDREGION}

{ TServiceReceiver }

constructor TJServiceReceiver._Create(Service: TSampleService);
begin
  inherited;
  Log.d('TJServiceReceiver._Create constructor');
  FOwningService := Service;
end;

class function TJServiceReceiver.Create(Service: TSampleService): JServiceReceiver;
begin
  Log.d('TJServiceReceiver.Create class function');
  Result := inherited Create;
  ServiceReceiver := TJServiceReceiver._Create(Service);
end;

procedure TJServiceReceiver.OnReceive(Context: JContext;
  ReceivedIntent: JIntent);
var
  Cmd: Integer;
begin
  Log.d('Service receiver received a message');
  Cmd := receivedIntent.getIntExtra(StringToJString(TSampleService.ID_INT_COMMAND), 0);
  if Cmd = TSampleService.CMD_STOP_SERVICE then
  begin
    Log.d('It was a STOP message');
    FOwningService.Running := false;
    Log.d('Service receiver is stopping service');
    FOwningService.GetJService.stopSelf;
    Toast('Service stopped by main activity', TToastLength.LongToast)
  end;
end;

initialization
  RegisterDelphiNativeMethods
end.
