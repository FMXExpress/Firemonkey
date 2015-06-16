program iOSFrame;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrame in 'uFrame.pas' {Frame1: TFrame},
  uFrame2 in 'uFrame2.pas' {Frame2: TFrame},
  uMain in 'uMain.pas' {FMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMainForm, FMainForm);
  Application.Run;
end.
