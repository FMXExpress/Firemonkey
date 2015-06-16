Program ANDTextView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FLabels};

{$I DPF.ANDROID.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFLabels, FLabels);
  Application.Run;

End.
