program Movie;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMovie in 'UMovie.pas' {Form1073};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm1073, Form1073);
  Application.Run;
end.
