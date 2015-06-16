program iOSSLCompose;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FSLComposeView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFSLComposeView, FSLComposeView);
  Application.Run;
end.
