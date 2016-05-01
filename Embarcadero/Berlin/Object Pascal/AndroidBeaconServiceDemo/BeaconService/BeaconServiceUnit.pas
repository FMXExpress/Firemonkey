//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit BeaconServiceUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os, System.Beacon, System.Notification, System.Beacon.Components,
  System.Bluetooth;

type
  TBeaconServiceDM = class(TAndroidService)
    Beacon1: TBeacon;
    NotificationCenter1: TNotificationCenter;
    procedure Beacon1BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure Beacon1BeaconEnter(const Sender: TObject; const ABeacon: IBeacon;
      const CurrentBeaconList: TBeaconList);
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

procedure TBeaconServiceDM.Beacon1BeaconEnter(const Sender: TObject;
  const ABeacon: IBeacon; const CurrentBeaconList: TBeaconList);
begin
  NotifyBeaconProximity('BE: ' + ABeacon.GUID.ToString + ':' + ABeacon.Major.ToString + ',' + ABeacon.Minor.ToString);
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
