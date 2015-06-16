Program iOSTimer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTimer};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFTimer, FTimer);
  Application.Run;

End.
