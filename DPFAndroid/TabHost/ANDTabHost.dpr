program ANDTabHost;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTabHost};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTabHost, FTabHost);
  Application.Run;
end.
