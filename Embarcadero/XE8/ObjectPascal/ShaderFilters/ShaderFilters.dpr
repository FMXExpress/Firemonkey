program ShaderFilters;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.RegisterFormFamily('TForm', [TfrmMain]);
  Application.Run;
end.
