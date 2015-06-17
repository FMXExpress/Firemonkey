program MessageAlerts;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {MessageAlertsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMessageAlertsForm, MessageAlertsForm);
  Application.Run;
end.
