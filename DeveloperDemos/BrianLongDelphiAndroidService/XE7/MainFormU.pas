unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Platform, ActivityReceiverU;

type
  TServiceInteractionForm = class(TForm)
    StopServiceButton: TButton;
    StartServiceButton: TButton;
    StartTimer: TTimer;
    ExitTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure ExitTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FReceiver: JActivityReceiver;
    FReceiverRegistered: Boolean;
    FServiceIsRunning: Boolean;
    procedure RegisterReceiver;
    procedure UnregisterReceiver;
    procedure StartService;
    procedure StopService;
  public
    { Public declarations }
    const SAMPLE_ACTIVITY_ACTION = 'SAMPLE_ACTIVITY_ACTION';
    var AppEvents: IFMXApplicationEventService;
    function ApplicationEventHandler(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
  end;

var
  ServiceInteractionForm: TServiceInteractionForm;

implementation

uses
  System.TypInfo,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, ServiceU;

{$R *.fmx}

function TServiceInteractionForm.ApplicationEventHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  Log.d('', Self, 'ApplicationEventHandler', Format('+ %s',
    [GetEnumName(TypeInfo(TApplicationEvent), Integer(AAppEvent))]));
  Result := True;
  // Clean up the UI display when app becomes active
  // or is brought back to the foreground
  case AAppEvent of
    TApplicationEvent.FinishedLaunching:
    begin
      //
    end;
    TApplicationEvent.BecameActive:
    begin
      RegisterReceiver
    end;
    TApplicationEvent.EnteredBackground:
    begin
      UnregisterReceiver
    end;
    TApplicationEvent.WillTerminate:
    begin
      //
    end;
  end;
  Log.d('', Self, 'ApplicationEventHandler', '-');
end;

procedure TServiceInteractionForm.FormCreate(Sender: TObject);
begin
  Log.d('TServiceInteractionForm.FormCreate');
  //Set up event that triggers when app is brought back to foreground
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXApplicationEventService,
       IInterface(AppEvents)) then
    AppEvents.SetApplicationEventHandler(ApplicationEventHandler);
  STartTimer.Enabled := True;
end;

procedure TServiceInteractionForm.FormDestroy(Sender: TObject);
begin
//  if FServiceIsRunning then
//  begin
//    Log.d('TServiceInteractionForm.FormDestroy triggering the stop service code');
//    StopService;
//    Sleep(5000);
//  end;
//  Log.d('TServiceInteractionForm.FormDestroy exiting');
  //After this point we crash
  //TODO: work out why
end;

procedure TServiceInteractionForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  Log.d('TServiceInteractionForm.FormKeyDown: Key = %d ($%0:x)', [Key]);
  if Key = vkHardwareBack then
  begin
    Log.d('Subverting the Back key to act like Home');
    Key := 0;
    Log.d('Stopping the service');
    StopService;
    Log.d('Moving task to the background');
    SharedActivity.moveTaskToBack(True);
    Log.d('Starting the exit timer');
    ExitTimer.Enabled := True;
  end;
end;

procedure TServiceInteractionForm.ExitTimerTimer(Sender: TObject);
begin
  Log.d('Exit timer has fired');
  ExitTimer.Enabled := False;
  Log.d('Terminating this activity');
  SharedActivity.finish;
end;

procedure TServiceInteractionForm.RegisterReceiver;
var
  Filter: JIntentFilter;
begin
  if not FReceiverRegistered and (FReceiver <> nil) then
  begin
    //Register the activity's broadcast receiver which will pick up messages from the service
    Filter := TJIntentFilter.Create;
    Filter.addAction(StringToJString(TSampleService.SAMPLE_SERVICE_ACTION));
    Log.d('Registering the activity receiver');
    SharedActivity.registerReceiver(FReceiver, Filter);
    FReceiverRegistered := True;
  end
end;

procedure TServiceInteractionForm.StartService;
var
  ServiceIntent: JIntent;
begin
  if not FServiceIsRunning then
  begin
    //Create the form's broadcast receiver
    Log.d('Creating activity receiver');
    FReceiver := TJActivityReceiver.Create;
    RegisterReceiver;
    //Start the service
    Log.d('Creating service intent');
    ServiceIntent := TJIntent.JavaClass.init(SharedActivityContext,
      TJLang_Class.JavaClass.forName(
        StringToJString('com.blong.test.SampleService'),
        True,
        SharedActivity.getClassLoader));
    Log.d('Service initiation intent created');
    ServiceIntent.putExtra(StringToJString(TSampleService.ID_INT_START_VALUE), Integer(10));
    Log.d('Starting service');
    if SharedActivity.startService(serviceIntent) = nil then
      Log.d('startService returned nil');
    FServiceIsRunning := True;
  end
end;

procedure TServiceInteractionForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService
end;

procedure TServiceInteractionForm.StartTimerTimer(Sender: TObject);
begin
  StartService;
  StartTimer.Enabled := False;
end;

procedure TServiceInteractionForm.StopService;
var
  Intent: JIntent;
begin
  if FServiceIsRunning then
  begin
    UnregisterReceiver;
    FReceiver := nil;
    Intent := TJIntent.Create;
    Intent.setAction(StringToJString(SAMPLE_ACTIVITY_ACTION));
    Intent.putExtra(StringToJString(TSampleService.ID_INT_COMMAND), TSampleService.CMD_STOP_SERVICE);
    Log.d('Broadcasting service stop intent');
    SharedActivity.sendBroadcast(Intent);
    FServiceIsRunning := False;
  end;
end;

procedure TServiceInteractionForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TServiceInteractionForm.UnregisterReceiver;
begin
  if FReceiverRegistered and (FReceiver <> nil) then
  begin
    //Unregister the broadcast receiver
    Log.d('Unregistering the activity receiver');
    SharedActivity.unregisterReceiver(FReceiver);
    FReceiverRegistered := False;
  end;
end;

end.
