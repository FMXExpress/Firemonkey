Program iOSAVAudioPlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FAVAudioPlayer};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFAVAudioPlayer, FAVAudioPlayer);
  Application.Run;

End.
