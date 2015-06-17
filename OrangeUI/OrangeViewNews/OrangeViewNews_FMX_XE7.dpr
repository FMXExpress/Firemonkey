program OrangeViewNews_FMX_XE7;













uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  XSuperJSON in '..\..\OrangeProjectCommon\XSuperObject\XSuperJSON.pas',
  XSuperObject in '..\..\OrangeProjectCommon\XSuperObject\XSuperObject.pas',
  LoadingFrame in 'LoadingFrame.pas' {FrameLoading: TFrame},
  MainForm in 'MainForm.pas' {frmMain},
  NewsDetailFrame in 'NewsDetailFrame.pas' {FrameNewsDetail: TFrame},
  NewsHomeFrame in 'NewsHomeFrame.pas' {FrameNewsHome: TFrame},
  NewsListFrame in 'NewsListFrame.pas' {FrameNewsList: TFrame},
  uDirectoryPublic in 'uDirectoryPublic.pas',
  uIdHttpControl in 'uIdHttpControl.pas',
  uInterfaceClass in 'uInterfaceClass.pas',
  uInterfaceCollection in 'uInterfaceCollection.pas',
  uInterfaceData in 'uInterfaceData.pas',
  uInterfaceManager in 'uInterfaceManager.pas',
  uPublic in 'uPublic.pas',
  uSkinPublic in 'uSkinPublic.pas',
  WaitingFrame in 'WaitingFrame.pas' {FrameWaiting: TFrame},
  Macapi.WebView in '..\..\OrangeProjectCommon\WebBrowserEx\Macapi.WebView.pas',
  FMX.WebBrowser.Mac in '..\..\OrangeProjectCommon\WebBrowserEx\FMX.WebBrowser.Mac.pas',
  FMX.WebBrowserEx in '..\..\OrangeProjectCommon\WebBrowserEx\FMX.WebBrowserEx.pas',
  FMX.WebBrowser.Win in '..\..\OrangeProjectCommon\WebBrowserEx\FMX.WebBrowser.Win.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
