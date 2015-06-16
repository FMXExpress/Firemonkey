program EmbeddedControlsDemo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form4};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
