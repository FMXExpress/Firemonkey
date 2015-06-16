Program iOSSegmentedControl;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FSegmentedControl};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFSegmentedControl, FSegmentedControl);
  Application.Run;

End.
