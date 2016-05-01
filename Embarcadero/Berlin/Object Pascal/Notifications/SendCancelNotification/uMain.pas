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
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Platform,
  System.Notification, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TNotificationsForm = class(TForm)
    btnSendScheduledNotification: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    btnSendNotificationImmediately: TButton;
    ToolBar2: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    NotificationC: TNotificationCenter;
    Memo1: TMemo;
    procedure btnSendScheduledNotificationClick(Sender: TObject);
    procedure btnSendNotificationImmediatelyClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure NotificationCReceiveLocalNotification(Sender: TObject;
      ANotification: TNotification);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NotificationsForm: TNotificationsForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
uses FMX.DialogService.Async;

procedure TNotificationsForm.btnSendNotificationImmediatelyClick(
  Sender: TObject);
var
  Notification: TNotification;
begin
  { verify if the service is actually supported }
  Notification := NotificationC.CreateNotification;
  try
    Notification.Name := 'MyNotification';
    Notification.AlertBody := 'Delphi for Mobile is here!';
    Notification.FireDate := Now;

    { Send notification in Notification Center }
    NotificationC.PresentNotification(Notification);
    { also this method is equivalent if platform supports scheduling }
    //NotificationC.ScheduleNotification(Notification);
  finally
    Notification.DisposeOf;
  end;
end;

procedure TNotificationsForm.btnSendScheduledNotificationClick(Sender: TObject);
var
  Notification: TNotification;
begin

{ verify if the service is actually supported }
  Notification := NotificationC.CreateNotification;
  try
    Notification.Name := 'MyNotification';
    Notification.AlertBody := 'Delphi for Mobile is here!';

    { Fired in 10 second }
    Notification.FireDate := Now + EncodeTime(0,0,10,0);

    { Send notification in Notification Center }
    NotificationC.ScheduleNotification(Notification);
  finally
    Notification.DisposeOf;
  end;
end;

procedure TNotificationsForm.NotificationCReceiveLocalNotification(
  Sender: TObject; ANotification: TNotification);
begin
  Memo1.Lines.Add(ANotification.AlertBody);
end;

procedure TNotificationsForm.SpeedButton1Click(Sender: TObject);
begin
    { providing the fact that you already have a MyNotification previously issued }
  NotificationC.CancelNotification('MyNotification');
end;

procedure TNotificationsForm.SpeedButton2Click(Sender: TObject);
begin
  NotificationC.CancelAll;
end;

end.
