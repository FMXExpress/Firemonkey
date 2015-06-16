program CameraComponent;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {CameraComponentForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCameraComponentForm, CameraComponentForm);
  Application.Run;
end.
