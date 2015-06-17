program PlayingAudio;

uses
  System.StartUpCopy,
  FMX.Forms,
  PlayAudioFile in 'PlayAudioFile.pas' {AudioPlayBackForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAudioPlayBackForm, AudioPlayBackForm);
  Application.Run;
end.
