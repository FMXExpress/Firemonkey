program ANDDeviceInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDeviceInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFDeviceInfo, FDeviceInfo);
  Application.Run;
end.
