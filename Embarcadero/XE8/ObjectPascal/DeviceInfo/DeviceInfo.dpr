program DeviceInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {DeviceInfoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDeviceInfoForm, DeviceInfoForm);
  Application.Run;
end.
