Program ANDTranslateAnimation;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FSlideAnimation};

{$I DPF.ANDROID.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFSlideAnimation, FSlideAnimation);
  Application.Run;

End.
