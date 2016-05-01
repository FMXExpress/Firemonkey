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
unit NotificationServiceUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os, System.Notification;

type
  TNotificationServiceDM = class(TAndroidService)
    NotificationCenter1: TNotificationCenter;
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    { Private declarations }
    FThread: TThread;
    procedure LaunchNotification;
  public
    { Public declarations }
  end;

var
  NotificationServiceDM: TNotificationServiceDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  Androidapi.JNI.App, System.DateUtils;

{$R *.dfm}

function TNotificationServiceDM.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags,
  StartId: Integer): Integer;
begin
  LaunchNotification;
  JavaService.stopSelf;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TNotificationServiceDM.LaunchNotification;
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'ServiceNotification';
    MyNotification.Title := 'Android Service Notification';
    MyNotification.AlertBody := 'RAD Studio 10 Seattle';
    MyNotification.FireDate := IncSecond(Now, 8);
    NotificationCenter1.ScheduleNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

end.
