
//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program SearchContext;

uses
  FMX.Forms,
  formMain in 'formMain.pas' {frmMain},
  unitSearchMenuHelper in 'unitSearchMenuHelper.pas',
  formData in 'formData.pas' {frmManageData},
  dataData in 'dataData.pas' {dtmdlData: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdtmdlData, dtmdlData);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
