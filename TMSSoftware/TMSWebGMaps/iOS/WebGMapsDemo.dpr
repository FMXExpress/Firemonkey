program WebGMapsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UWebGMapsDemo in 'UWebGMapsDemo.pas' {Form65};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm65, Form65);
  Application.Run;
end.
