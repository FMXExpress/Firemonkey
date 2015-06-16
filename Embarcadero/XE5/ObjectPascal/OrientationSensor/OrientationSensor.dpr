program OrientationSensor;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {OrientationSensorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait];
  Application.CreateForm(TOrientationSensorForm, OrientationSensorForm);
  Application.Run;
end.
