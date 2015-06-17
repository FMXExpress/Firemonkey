program WeiBo_FMX_XE8;













uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MobileSelPublishTypeFrame in 'MobileSelPublishTypeFrame.pas' {FrameMobileSelPublishType: TFrame},
  MobileAuthFrame in 'MobileAuthFrame.pas' {FrameMobileAuth: TFrame},
  MobileHomeFrame in 'MobileHomeFrame.pas' {FrameMobileHome: TFrame},
  MobileMainForm in 'MobileMainForm.pas' {frmMobileMain},
  MobileMainFrame in 'MobileMainFrame.pas' {FrameMobileMain: TFrame},
  MobileMyFrame in 'MobileMyFrame.pas' {FrameMobileMy: TFrame},
  uAPIItem_users_counts in 'WeiBoAPI\API\用户\读取接口\uAPIItem_users_counts.pas',
  uAPIItem_users_show in 'WeiBoAPI\API\用户\读取接口\uAPIItem_users_show.pas',
  uAPIItem_statuses_public_timeline in 'WeiBoAPI\API\微博\读取接口\uAPIItem_statuses_public_timeline.pas',
  uAPIItem_statuses_repost_timeline in 'WeiBoAPI\API\微博\读取接口\uAPIItem_statuses_repost_timeline.pas',
  uAPIItem_statuses_show in 'WeiBoAPI\API\微博\读取接口\uAPIItem_statuses_show.pas',
  uAPIItem_comments_show in 'WeiBoAPI\API\微博\读取接口\uAPIItem_comments_show.pas',
  uAPIItem_statuses_home_timeline in 'WeiBoAPI\API\微博\读取接口\uAPIItem_statuses_home_timeline.pas',
  uAPIItem_statuses_update in 'WeiBoAPI\API\微博\写入接口\uAPIItem_statuses_update.pas',
  uAPIItem_oauth2_access_token in 'WeiBoAPI\API\授权\uAPIItem_oauth2_access_token.pas',
  uAPIItem_oauth2_authorize in 'WeiBoAPI\API\授权\uAPIItem_oauth2_authorize.pas',
  uIdHttpControl in 'WeiBoAPI\OpenPlatform\uIdHttpControl.pas',
  uOpenPlatform in 'WeiBoAPI\OpenPlatform\uOpenPlatform.pas',
  uPublic in 'WeiBoAPI\OpenPlatform\uPublic.pas',
  uUrlParam in 'WeiBoAPI\OpenPlatform\uUrlParam.pas',
  uDataStructure in 'WeiBoAPI\OpenPlatform\uDataStructure.pas',
  uWeiboUtils in 'uWeiboUtils.pas',
  uSinaWeiboManager in 'uSinaWeiboManager.pas',
  MobileFindFrame in 'MobileFindFrame.pas' {FrameMobileFind: TFrame},
  MobileLoginFrame in 'MobileLoginFrame.pas' {FrameMobileLogin: TFrame},
  MobileMsgFrame in 'MobileMsgFrame.pas' {FramelMobileMsg: TFrame},
  FMX.Platform.iOS in '..\..\OrangeProjectCommon\FMX.Platform.iOS.pas',
  uMobileUtils in '..\..\OrangeProjectCommon\uMobileUtils.pas',
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  FMX.WebBrowserEx in '..\..\OrangeProjectCommon\WebBrowserEx\FMX.WebBrowserEx.pas',
  Macapi.WebView in '..\..\OrangeProjectCommon\WebBrowserEx\Macapi.WebView.pas',
  FMX.WebBrowser.Mac in '..\..\OrangeProjectCommon\WebBrowserEx\FMX.WebBrowser.Mac.pas',
  XSuperObject in '..\..\OrangeProjectCommon\XSuperObject\XSuperObject.pas',
  XSuperJSON in '..\..\OrangeProjectCommon\XSuperObject\XSuperJSON.pas';

{$R *.res}

begin
//  ReportMemoryLeaksONShutdown:=DebugHook<>0;
  Application.Initialize;
  Application.CreateForm(TfrmMobileMain, frmMobileMain);
  Application.Run;
end.
