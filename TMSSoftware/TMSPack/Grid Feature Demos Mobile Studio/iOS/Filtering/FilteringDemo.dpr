program FilteringDemo;

uses
  FMX.Forms,
  FMX.Types,
  UDemo in 'UDemo.pas' {Form719};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape];
  Application.CreateForm(TForm719, Form719);
  Application.Run;
end.
