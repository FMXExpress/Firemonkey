Program iOSDrawingB;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDrawingBitMap};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFDrawingBitMap, FDrawingBitMap);
  Application.Run;

End.
