program iOSSlideDialog;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FAlertMessage};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFAlertMessage, FAlertMessage);
  Application.Run;
end.
