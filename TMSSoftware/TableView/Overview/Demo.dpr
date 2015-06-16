program Demo;

uses
  FMX.Forms,
  FMX.Types,
  UDemo in 'UDemo.pas' {Form536};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  Application.Initialize;
  Application.CreateForm(TForm536, Form536);
  Application.Run;
end.
