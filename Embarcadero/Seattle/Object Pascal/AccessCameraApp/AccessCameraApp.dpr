program AccessCameraApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {AccessCameraAppForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAccessCameraAppForm, AccessCameraAppForm);
  Application.Run;
end.
