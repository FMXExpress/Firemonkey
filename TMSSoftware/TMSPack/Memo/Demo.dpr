program Demo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form832};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm832, Form832);
  Application.Run;
end.
