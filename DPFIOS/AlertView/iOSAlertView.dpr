Program iOSAlertView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FAlertView};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFAlertView, FAlertView);
  Application.Run;

End.
