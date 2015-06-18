program SensorTag;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FrMainform};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrMainform, FrMainform);
  Application.Run;
end.
