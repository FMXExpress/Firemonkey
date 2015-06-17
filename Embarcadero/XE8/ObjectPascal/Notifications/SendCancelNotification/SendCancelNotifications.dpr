program SendCancelNotifications;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {NotificationsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotificationsForm, NotificationsForm);
  Application.Run;
end.
