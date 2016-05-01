program NotificationService;

uses
  System.Android.ServiceApplication,
  NotificationServiceUnit in 'NotificationServiceUnit.pas' {NotificationServiceDM: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNotificationServiceDM, NotificationServiceDM);
  Application.Run;
end.
