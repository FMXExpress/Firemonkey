program ImageRotation;

uses
  System.StartUpCopy,
  FMX.Forms,
  ImageRotationU in 'ImageRotationU.pas' {ImageRotationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageRotationForm, ImageRotationForm);
  Application.Run;
end.
