program iOSPopover;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPopover},
  uFrame in 'uFrame.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPopover, FPopover);
  Application.Run;
end.
