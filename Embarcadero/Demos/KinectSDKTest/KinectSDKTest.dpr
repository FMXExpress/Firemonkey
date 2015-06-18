program KinectSDKTest;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  NuiApi in 'NuiApi.pas',
  NuiImageCamera in 'NuiImageCamera.pas',
  NuiSensor in 'NuiSensor.pas',
  NuiSkeleton in 'NuiSkeleton.pas',
  EventDispatcherThread in 'EventDispatcherThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
