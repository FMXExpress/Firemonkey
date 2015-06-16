program TwitterDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UTwitterDemo in 'UTwitterDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
