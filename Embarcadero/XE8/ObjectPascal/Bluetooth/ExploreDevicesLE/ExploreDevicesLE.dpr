program ExploreDevicesLE;

uses
  System.StartUpCopy,
  FMX.Forms,
  uExploreDevices in 'uExploreDevices.pas' {FrDeviceExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrDeviceExplorer, FrDeviceExplorer);
  Application.Run;
end.
