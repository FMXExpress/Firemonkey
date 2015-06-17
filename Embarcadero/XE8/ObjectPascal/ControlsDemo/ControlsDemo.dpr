program ControlsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ctrlsdemofrm in 'ctrlsdemofrm.pas' {frmCtrlsDemo},
  aboutboxfrm in 'aboutboxfrm.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCtrlsDemo, frmCtrlsDemo);
  Application.Run;
end.
