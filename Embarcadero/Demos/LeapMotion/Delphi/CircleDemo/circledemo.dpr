program circledemo;

uses
  Forms, 
  frmmain in 'frmmain.pas' {form1};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

