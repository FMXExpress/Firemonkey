program iOSHttpContentAuth;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FHTTPContent};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFHTTPContent, FHTTPContent);
  Application.Run;
end.
