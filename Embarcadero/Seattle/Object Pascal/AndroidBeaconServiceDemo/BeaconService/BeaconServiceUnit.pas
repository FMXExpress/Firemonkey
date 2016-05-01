unit BeaconServiceUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os, System.Beacon, System.Notification, System.Beacon.Components;

type
  TBeaconServiceDM = class(TAndroidService)
    Beacon1: TBeacon;
    NotificationCenter1: TNotificationCenter;
    procedure Beacon1BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    { Private declarations }
    procedure NotifyBeaconProximity(const BeaconName: string);
  public
    { Public declarations }
  end;

var
  BeaconServiceDM: TBeaconServiceDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.JNI.App;

function TBeaconServiceDM.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags,
  StartId: Integer): Integer;
begin
  Beacon1.Enabled := True;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TBeaconServiceDM.Beacon1BeaconProximity(const Sender: TObject; const ABeacon: IBeacon;
  Proximity: TBeaconProximity);
begin
  if Proximity = TBeaconProximity.Immediate then
    NotifyBeaconProximity(ABeacon.GUID.ToString + ':' + ABeacon.Major.ToString + ',' + ABeacon.Minor.ToString);
end;

procedure TBeaconServiceDM.NotifyBeaconProximity(const BeaconName: string);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'BeaconProximityNotification';
    MyNotification.AlertBody := 'Beacon Proximity detected: '+  BeaconName;
    NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

end.
