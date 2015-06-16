Program iOSVCO;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FViewControllerOverride};

{$R *.res}

Begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait, TFormOrientation.soInvertedPortrait, TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TFViewControllerOverride, FViewControllerOverride);
  Application.Run;

End.
