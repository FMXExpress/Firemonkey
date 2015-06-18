program tapdemo;

uses
  Forms,
  frmmain in 'frmmain.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

