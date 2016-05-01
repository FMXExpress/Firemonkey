program CustomAdapter;

uses
  System.StartUpCopy,
  FMX.Forms,
  Customnik in 'Customnik.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
