program GuessGameClient;

uses
  FMX.Forms,
  formMain in 'formMain.pas' {frmMain},
  unitGame in 'unitGame.pas',
  unitGamePlayer in 'unitGamePlayer.pas',
  dmMain in 'dmMain.pas' {dataMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdataMain, dataMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
