program BeaconDeviceDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UBeaconDevDemo in 'UBeaconDevDemo.pas' {Form37};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm37, Form37);
  Application.Run;
end.
