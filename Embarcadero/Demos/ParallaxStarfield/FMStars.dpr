program FMStars;

uses
  FMX_Forms,
  FormStarsUnit in 'FormStarsUnit.pas' {Form1},
  AllStars in 'AllStars.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
