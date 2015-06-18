program SearchContext;

uses
  System.StartUpCopy,
  FMX.Forms,
  unitSearchMenuHelper in 'unitSearchMenuHelper.pas',
  formMain in 'formMain.pas' {frmMain},
  formData in 'formData.pas' {frmManageData},
  dataData in 'dataData.pas' {dtmdlData: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdtmdlData, dtmdlData);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmManageData, frmManageData);
  Application.Run;
end.
