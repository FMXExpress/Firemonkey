program CustomListBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  customlistfrm in 'customlistfrm.pas' {frmCustomList};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCustomList, frmCustomList);
  Application.RegisterFormFamily('TForm', [TfrmCustomList]);
  Application.Run;
end.
