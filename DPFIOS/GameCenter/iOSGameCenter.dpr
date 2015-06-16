Program iOSGameCenter;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FGameCenter};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFGameCenter, FGameCenter);
  Application.Run;

End.
