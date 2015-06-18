program ProximityServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
