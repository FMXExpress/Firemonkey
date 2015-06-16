Program iOSButtons;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FButtons};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFButtons, FButtons);
  Application.Run;

End.
