program VideoCaptureHD;

uses
  System.StartUpCopy,
  FMX.Forms,
  CaptureForm in 'CaptureForm.pas' {Form240};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm240, Form240);
  Application.Run;
end.
