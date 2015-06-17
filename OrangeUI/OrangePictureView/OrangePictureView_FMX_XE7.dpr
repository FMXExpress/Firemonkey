program OrangePictureView_FMX_XE7;



uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ViewPictureListFrame in 'ViewPictureListFrame.pas' {FrameViewPictureList: TFrame},
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
