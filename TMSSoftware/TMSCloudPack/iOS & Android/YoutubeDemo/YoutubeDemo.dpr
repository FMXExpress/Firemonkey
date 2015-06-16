program YoutubeDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UYoutubeDemo in 'UYoutubeDemo.pas' {Form1171};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm1171, Form1171);
  Application.Run;
end.
