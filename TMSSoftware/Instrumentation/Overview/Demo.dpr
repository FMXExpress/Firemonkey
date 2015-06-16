program Demo;

uses
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form472};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm472, Form472);
  Application.Run;
end.
