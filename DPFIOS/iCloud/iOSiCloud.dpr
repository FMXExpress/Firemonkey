program iOSiCloud;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FiCloud};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFiCloud, FiCloud);
  Application.Run;
end.
