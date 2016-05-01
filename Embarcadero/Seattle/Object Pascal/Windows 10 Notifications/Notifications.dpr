program Notifications;

uses
  System.StartUpCopy,
  FMX.Forms,
  NotificationsForm in 'NotificationsForm.pas' {Notify};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotify, Notify);
  Application.Run;
end.
