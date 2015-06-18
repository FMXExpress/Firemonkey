program GuessGameMobile;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MobileMain in 'MobileMain.pas' {frmMobileGuessGame},
  dmMain in '..\Client\dmMain.pas' {dataMain: TDataModule},
  unitGamePlayer in '..\Client\unitGamePlayer.pas',
  unitGame in '..\Client\unitGame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMobileGuessGame, frmMobileGuessGame);
  Application.CreateForm(TdataMain, dataMain);
  Application.Run;
end.
