program Demo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form676};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm676, Form676);
  Application.Run;
end.
