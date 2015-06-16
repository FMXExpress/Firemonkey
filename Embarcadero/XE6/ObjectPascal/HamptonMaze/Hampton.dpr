program Hampton;

uses
  FMX.Forms,
  Hampton_U in 'Hampton_U.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
