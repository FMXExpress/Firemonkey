program iOSSlideUpDown;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FSlideUpDown},
  uFrame1 in 'uFrame1.pas' {Frame1: TFrame},
  uFrame2 in 'uFrame2.pas' {Frame2: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFSlideUpDown, FSlideUpDown);
  Application.Run;
end.
