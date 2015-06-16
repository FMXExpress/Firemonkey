program Demo;

uses
  FMX.Forms,
  FMX.Types,
  UDemo in 'UDemo.pas' {Form738};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm738, Form738);
  Application.Run;
end.
