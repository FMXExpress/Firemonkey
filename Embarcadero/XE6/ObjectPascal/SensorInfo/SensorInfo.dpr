program SensorInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uSensorInfo in 'uSensorInfo.pas' {frmAboutSensors};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAboutSensors, frmAboutSensors);
  Application.Run;
end.
