program BibleBuddie;

uses
  System.StartUpCopy,
  FMX.Forms,
  formMain in 'formMain.pas' {frmMain},
  dmMain in 'dmMain.pas' {dtmdlMain: TDataModule},
  BackButtonManager in 'BackButtonManager.pas',
  unitSettings in 'unitSettings.pas',
  formSplash in 'formSplash.pas' {frmSplash};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF ANDROID}
  Application.CreateForm(TdtmdlMain, dtmdlMain);
  Application.CreateForm(TfrmMain, frmMain);
  {$ELSE}
  Application.CreateForm(TfrmSplash, frmSplash);
  {$ENDIF}
  Application.Run;
end.
