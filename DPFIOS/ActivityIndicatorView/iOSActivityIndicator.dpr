Program iOSActivityIndicator;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {ActivityIndicator};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TActivityIndicator, ActivityIndicator);
  Application.Run;

End.
