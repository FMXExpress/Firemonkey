program EchoClassic;

uses
  System.StartUpCopy,
  FMX.Forms,
  EchoClassicForm in 'EchoClassicForm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTEchoClassicForm, TEchoClassicForm);
  Application.Run;
end.
