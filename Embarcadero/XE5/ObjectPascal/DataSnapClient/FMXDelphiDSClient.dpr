program FMXDelphiDSClient;

uses
  FMX.Forms,
  FormMain in 'FormMain.pas' {FrmMain},
  Login in 'Login.pas' {FrmLogin},
  DMMain in 'DMMain.pas' {DM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
