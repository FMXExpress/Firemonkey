program HttpDownloadManager;

uses
  System.StartUpCopy,
  FMX.Forms,
  uDownloadManager in 'uDownloadManager.pas' {FrDownloadManager},
  uDownloadThread in 'uDownloadThread.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFrDownloadManager, FrDownloadManager);
  Application.Run;
end.
