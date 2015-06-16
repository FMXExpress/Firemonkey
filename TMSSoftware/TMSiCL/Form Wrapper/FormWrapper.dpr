program FormWrapper;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFormWrapper in 'UFormWrapper.pas' {Form936},
  UFormSample in 'UFormSample.pas' {Form1},
  UFormSample2 in 'UFormSample2.pas' {Form2},
  UFormSample3 in 'UFormSample3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm936, Form936);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
