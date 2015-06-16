program iOSPainter;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDrawingNative};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFDrawingNative, FDrawingNative);
  Application.Run;
end.
