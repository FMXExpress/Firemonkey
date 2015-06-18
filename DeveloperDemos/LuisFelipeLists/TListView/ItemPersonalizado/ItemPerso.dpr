program ItemPerso;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
