Program ANDTextSwitcher;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTextSwitcher};

{$I DPF.ANDROID.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFTextSwitcher, FTextSwitcher);
  Application.Run;

End.
