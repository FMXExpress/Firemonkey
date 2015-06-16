program ANDHTTPUpload;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FHTTPUpload};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFHTTPUpload, FHTTPUpload);
  Application.Run;
end.
