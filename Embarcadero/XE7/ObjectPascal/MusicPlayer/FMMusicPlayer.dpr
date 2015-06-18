program FMMusicPlayer;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MediaPlayerU in 'MediaPlayerU.pas' {FMXMusicPlayerFrm},
  MusicPlayer.Android in 'MusicPlayer.Android.pas',
  MusicPlayer.iOS in 'MusicPlayer.iOS.pas',
  MusicPlayer.Utils in 'MusicPlayer.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXMusicPlayerFrm, FMXMusicPlayerFrm);
  Application.Run;
end.
