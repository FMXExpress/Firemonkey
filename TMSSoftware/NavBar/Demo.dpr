program Demo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form10};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
