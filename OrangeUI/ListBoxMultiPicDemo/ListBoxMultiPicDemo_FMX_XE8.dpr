program ListBoxMultiPicDemo_FMX_XE8;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  Unit1 in 'Unit1.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
//  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
