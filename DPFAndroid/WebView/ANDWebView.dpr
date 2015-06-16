program ANDWebView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FWebview};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFWebview, FWebview);
  Application.Run;
end.
