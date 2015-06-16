Program iOSAVAudioRecorder;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {AVAudioRecorder};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TAVAudioRecorder, AVAudioRecorder);
  Application.Run;

End.
