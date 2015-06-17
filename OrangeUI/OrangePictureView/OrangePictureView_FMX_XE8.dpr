program OrangePictureView_FMX_XE8;



uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ViewPictureListFrame in 'ViewPictureListFrame.pas' {FrameViewPictureList: TFrame},
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  FMX.Platform.iOS in '..\..\OrangeProjectCommon\FMX.Platform.iOS.pas';

{$R *.res}

begin
//  ReportMemoryLeaksONShutdown:=DebugHook<>0;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
