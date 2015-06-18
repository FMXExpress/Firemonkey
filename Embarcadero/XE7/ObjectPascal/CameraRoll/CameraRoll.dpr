program CameraRoll;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {CameraRollForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCameraRollForm, CameraRollForm);
  Application.CreateForm(TCameraRollForm, CameraRollForm);
  Application.Run;
end.
