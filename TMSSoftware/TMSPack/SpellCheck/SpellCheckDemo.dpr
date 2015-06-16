program SpellCheckDemo;

uses
  FMX.Forms,
  Uspellcheckdemo in 'Uspellcheckdemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
