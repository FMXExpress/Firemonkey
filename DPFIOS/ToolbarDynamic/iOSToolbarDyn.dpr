Program iOSToolbarDyn;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FiOSToolbarDyn};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFiOSToolbarDyn, FiOSToolbarDyn);
  Application.Run;

End.
