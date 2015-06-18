program VLB_objects;

uses
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uPerson in 'uPerson.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
