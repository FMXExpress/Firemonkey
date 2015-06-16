program iOSHttpPut;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FHTTPPostData};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFHTTPPostData, FHTTPPostData);
  Application.Run;
end.
