program ANDHTTPDownload;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FHTTPDownload};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFHTTPDownload, FHTTPDownload);
  Application.Run;
end.

