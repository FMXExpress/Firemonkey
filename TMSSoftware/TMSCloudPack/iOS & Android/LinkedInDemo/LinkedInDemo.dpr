program LinkedInDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ULinkedInDemo in 'ULinkedInDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
