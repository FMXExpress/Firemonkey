program iOSVideoCap;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FiOSVideoCap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFiOSVideoCap, FiOSVideoCap);
  Application.Run;
end.
