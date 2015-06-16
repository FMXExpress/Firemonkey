program Demo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form610};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm610, Form610);
  Application.Run;
end.
