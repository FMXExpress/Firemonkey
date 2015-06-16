program Demo;

uses
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form550};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm550, Form550);
  Application.Run;
end.
