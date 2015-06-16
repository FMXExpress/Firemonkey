Program iOSLocalAuthentication;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FLocalAuthentication};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFLocalAuthentication, FLocalAuthentication);
  Application.Run;

End.
