program Directions;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDirections in 'UDirections.pas' {Form1135};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm1135, Form1135);
  Application.Run;
end.
