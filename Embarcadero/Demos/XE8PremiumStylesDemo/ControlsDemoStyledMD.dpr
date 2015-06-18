program ControlsDemoStyledMD;

uses
  System.StartUpCopy,
  FMX.Forms,
  aboutboxfrm in 'aboutboxfrm.pas' {frmAbout},
  ctrlsdemofrm in 'ctrlsdemofrm.pas' {frmCtrlsDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCtrlsDemo, frmCtrlsDemo);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.RegisterFormFamily('TForm', [TfrmCtrlsDemo, TfrmAbout]);
  Application.Run;
end.
