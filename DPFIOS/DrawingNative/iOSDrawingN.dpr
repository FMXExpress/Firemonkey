Program iOSDrawingN;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDrawingNative};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFDrawingNative, FDrawingNative);
  Application.Run;

End.
