program InheritanceAndInterfaceDemo;

uses
  FMX.Forms,
  formMain in 'formMain.pas' {Form1},
  unitMyClasses in 'unitMyClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
