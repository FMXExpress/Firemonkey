unit ServiceU;

interface

uses
  System.Classes,
  FMX.Types,
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

type
  TServiceThread = class;

  TSampleService = class
  private
    FReceiver: JBroadcastReceiver;
    FThread: TServiceThread;
    FSampleJService: JService;
  public
    const SAMPLE_SERVICE_ACTION = 'SAMPLE_SERVICE_ACTION';
    const ID_INT_START_VALUE = 'START_VALUE';
    const ID_INT_ITERATION = 'ITERATION';
    const ID_INT_CALCULATED_VALUE = 'CALCULATED_VALUE';
    const ID_INT_COMMAND = 'COMMAND';
    const CMD_STOP_SERVICE: Integer = 1;

    var Running: Boolean;
    var StartValue: Integer;

    constructor Create;
    function GetJService: JService;
    procedure OnDestroy;
    function OnStartCommand(StartIntent: JIntent; Flags, StartID: Integer): Integer;
  end;

  TServiceThread = class(TThread)
  public
    procedure Execute; override;
  end;

implementation

uses
  System.SysUtils,
  FMX.Helpers.Android,
  FMX.Platform.Android,
  Androidapi.Helpers,
  Androidapi.NativeActivity,
  Androidapi.JNI,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Toast,
  ServiceReceiverU, MainFormU;

{$REGION 'JNI setup code and callback'}
var
  SampleService: TSampleService;
  SampleServiceObjectID: JNIObject;

//This is called from the Java activity's onStartCommand() method
function SampleServiceOnStartCommandNative(PEnv: PJNIEnv; This: JNIObject;
  JNIStartIntent: JNIObject; Flags, StartID: Integer): Integer; cdecl;
var
  MyResult: Integer;
begin
  Log.d('+SampleServiceOnStartCommandNative');
  Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId]);
  SampleServiceObjectID := This;
  Log.d('Calling Synchronize');
  TThread.Synchronize(nil,
    procedure
    begin
      MyResult := SampleService.OnStartCommand(TJIntent.Wrap(JNIStartIntent), Flags, StartID);
      Log.d('Synchronized method is over');
    end);
  Log.d('Synchronize is over');
  Result := MyResult;
  Log.d('-SampleServiceOnStartCommandNative');
end;

//This is called from the Java activity's onDestroy() method
procedure SampleServiceOnDestroyNative(PEnv: PJNIEnv; This: JNIObject); cdecl;
begin
  Log.d('+SampleServiceOnDestroyNative');
  Log.d('Thread: Main: %.8x, Current: %.8x, Java:%.8d (%2:.8x)',
    [MainThreadID, TThread.CurrentThread.ThreadID,
    TJThread.JavaClass.CurrentThread.getId]);
  SampleServiceObjectID := This;
  //Not much mileage in synchronizing this - we're just destroying Java objects anyway
  SampleService.OnDestroy;
  //SampleService := nil;
  SampleServiceObjectID := nil;
  Log.d('-SampleServiceOnDestroyNative');
end;

procedure RegisterDelphiNativeMethods;
var
  PEnv: PJNIEnv;
  ServiceClass: JNIClass;
  NativeMethods: array[0..1] of JNINativeMethod;
begin
  Log.d('Starting the Sample Service JNI stuff');

  PEnv := TJNIResolver.GetJNIEnv;

  Log.d('Registering interop methods');

  NativeMethods[0].Name := 'sampleServiceOnStartCommandNative';
  NativeMethods[0].Signature := '(Landroid/content/Intent;II)I';
  NativeMethods[0].FnPtr := @SampleServiceOnStartCommandNative;
  NativeMethods[1].Name := 'sampleServiceOnDestroyNative';
  NativeMethods[1].Signature := '()V';
  NativeMethods[1].FnPtr := @SampleServiceOnDestroyNative;

  ServiceClass := TJNIResolver.GetJavaClassID('com.blong.test.SampleService');

  PEnv^.RegisterNatives(PEnv, ServiceClass, @NativeMethods[0], Length(NativeMethods));

  PEnv^.DeleteLocalRef(PEnv, ServiceClass);
end;
{$ENDREGION}

{ TJSampleService }

function TSampleService.GetJService: JService;
begin
  Log.d('TSampleService.GetJService');
  if FSampleJService = nil then
  begin
    Log.d('Wrapping service Object ID into a JService');
    FSampleJService := TJService.Wrap(SampleServiceObjectID)
  end;
  Result := FSampleJService;
end;

procedure TSampleService.OnDestroy;
begin
  Log.d('TJSampleService.OnDestroy');
  Log.d('Unregistering service receiver');
  //Shut down the service's broadcast receiver
  SharedActivity.unregisterReceiver(FReceiver);
  FThread := nil;
end;

function TSampleService.OnStartCommand(StartIntent: JIntent; Flags,
  StartID: Integer): Integer;
var
  Filter: JIntentFilter;
begin
  Log.d('TJSampleService.OnStartCommand');
  //Bear in mind the activity could call startService more than once, so we need to filter out subsequent invocations
  if not Running then
  begin
    Log.d('Service wasn''t running. Creating intent filter for receiver');
    //Register the service's broadcast receiver which will pick up messages from the activity
    Filter := TJIntentFilter.Create;
    Filter.addAction(StringToJString(TServiceInteractionForm.SAMPLE_ACTIVITY_ACTION));
    Log.d('Registering service receiver');
    SharedActivity.registerReceiver(FReceiver, Filter);
    //Now get the service thread up and running
    Running := true;
    Log.d('Retrieving start command from start intent');
    //Extract information passed along to the service
    StartValue := StartIntent.getIntExtra(StringToJString(ID_INT_START_VALUE), 1);
    //Kickstart the service worker thread
    Log.d('Starting service thread');
    FThread := TServiceThread.Create(False);
  end;
  Log.d('Setting thread to be START_STICKY');
  Result := TJService.JavaClass.START_STICKY;
end;

constructor TSampleService.Create;
begin
  inherited;
  Log.d('TJSampleService.Create constructor');
  //Create the service's broadcast receiver
  FReceiver := TJServiceReceiver.Create(Self);
end;

{ TServiceThread }

procedure TServiceThread.Execute;
var
  I: Integer;
  ActivityIntent: JIntent;
  Service: JService;
begin
  Log.d('TServiceThread.Execute');
  try
    I := 1;
    Service := TJService.Wrap(SampleServiceObjectID);
    //We don't need to keep a class field that points to
    //the service since there's a global pointing to it
    while (SampleService <> nil) and SampleService.Running do
      try
        Log.d('Service thread sees service running so off to sleep...');
        Sleep(5000);
        if (SampleService <> nil) and SampleService.Running then
        begin
          Log.d('Service thread setting up activity intent');
          ActivityIntent := TJIntent.Create;
          ActivityIntent.setAction(StringToJString(TSampleService.SAMPLE_SERVICE_ACTION));
          //Package up a couple of pieces of information in the intent message
          ActivityIntent.putExtra(StringToJString(TSampleService.ID_INT_ITERATION), i);
          ActivityIntent.putExtra(
          StringToJString(TSampleService.ID_INT_CALCULATED_VALUE),
          i * SampleService.StartValue);
          Log.d('Service thread launching activity intent');
          //This wrapped Java object is ted to the FMX thread, so we'll make another one in this thread
          //SampleService.GetJService.sendBroadcast(ActivityIntent);
          Service.sendBroadcast(ActivityIntent);
          Log.d('Sent a message from service to activity');
        end
        else
          Log.d('Service thread sees that the service has exited');
        Inc(i);
      except
        on Exception do
          //nothing
      end;
    Service := nil;
    if SampleService <> nil then
    begin
      Log.d('Service thread stopping service');
      SampleService.GetJService.stopSelf;
    end;
  finally
    Log.d('Service thread detaching from JNI');
    //Used a JNI wrapper, which will have attached the thread to Java
    //Now we must undo that
    if (GetAndroidApp <> nil) and (GetAndroidApp.activity <> nil) then
      GetAndroidApp.activity.vm^.DetachCurrentThread(GetAndroidApp.activity.vm);
    Log.d('Service thread exiting');
  end;
end;

initialization
  //We have a bridge class but we are actually creating an instance of that
  //class here, not using a class function to construct a Java object.
  //Service objects are created by the system, not by us, so we have no need
  //to create a Java service object. We'l just create a Delphi shell for it
  SampleService := TSampleService.Create;
  RegisterDelphiNativeMethods
end.
