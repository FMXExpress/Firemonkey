Program iOSToolbar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FToolbar};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFToolbar, FToolbar);
  Application.Run;

End.
