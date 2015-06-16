Program iOSFBSlidMenu;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPanGestureRecognizer};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFPanGestureRecognizer, FPanGestureRecognizer);
  Application.Run;

End.
