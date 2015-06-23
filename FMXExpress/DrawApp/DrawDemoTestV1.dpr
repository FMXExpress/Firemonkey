program DrawDemoTestV1;

uses
  System.StartUpCopy,
  FMX.Forms,
  MyPaintBox in 'MyPaintBox.pas',
  frmdrawmaindemo1 in 'frmdrawmaindemo1.pas' {drawmaindemo1frm},
  frmfilebrowseopen in 'frmfilebrowseopen.pas' {filebrowseopenfrm},
  frmSelectBmp in 'frmSelectBmp.pas' {SelectBmpfrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tdrawmaindemo1frm, drawmaindemo1frm);
  Application.CreateForm(TSelectBmpfrm, SelectBmpfrm);
  Application.CreateForm(Tfilebrowseopenfrm, filebrowseopenfrm);
  Application.Run;
end.
