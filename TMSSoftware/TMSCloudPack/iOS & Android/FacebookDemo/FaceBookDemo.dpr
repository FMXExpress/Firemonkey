program FaceBookDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFaceBookDemo in 'UFaceBookDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
