program VideoPlayback;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoForm in 'VideoForm.pas' {VideoPlayBackForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVideoPlayBackForm, VideoPlayBackForm);
  Application.Run;
end.
