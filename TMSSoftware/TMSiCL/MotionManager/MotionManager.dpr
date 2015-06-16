program MotionManager;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMotionManager in 'UMotionManager.pas' {Form1185};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1185, Form1185);
  Application.Run;
end.
