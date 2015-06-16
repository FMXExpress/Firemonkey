program InstagramDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UInstagramDemo in 'UInstagramDemo.pas' {Form1017};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm1017, Form1017);
  Application.Run;
end.
