program FourSquareDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFourSquareDemo in 'UFourSquareDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
