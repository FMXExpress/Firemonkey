program FireDAC_SQLite;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FireDAC_SQLiteForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFireDAC_SQLiteForm, FireDAC_SQLiteForm);
  Application.Run;
end.
