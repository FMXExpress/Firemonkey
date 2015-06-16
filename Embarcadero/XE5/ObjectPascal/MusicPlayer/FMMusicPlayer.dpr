program FMMusicPlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  MediaPlayerU in 'MediaPlayerU.pas' {FMXMusicPlayerFrm},
  MusicPlayer.iOS in 'MusicPlayer.iOS.pas',
  MusicPlayer.Android in 'MusicPlayer.Android.pas',
  MusicPlayer.Utils in 'MusicPlayer.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXMusicPlayerFrm, FMXMusicPlayerFrm);
  Application.Run;
end.
