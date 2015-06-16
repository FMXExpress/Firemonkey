program FourSquareDemo;

uses
  FMX.Forms,
  UCategories in 'UCategories.pas' {Form2},
  UFourSquareDemo in 'UFourSquareDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
