unit MobileAuthFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowserEx, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uUIFunction,
  uUrlParam,
  uBaseLog,
  uOpenPlatform,
//  ConfigForm,
  uFuncCommon,
  uFileCommon,
  uSinaWeiboManager,
  uAPIItem_oauth2_authorize,
  uAPIItem_oauth2_access_token, uSkinFireMonkeyButton, uSkinFireMonkeyImage;

type
  TFrameMobileAuth = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    imgToolBarDevide: TSkinFMXImage;
    procedure btnAuthClick(Sender: TObject);
  private
    FWebBrowser:TWebBrowserEx;
    procedure DoWebBrowserDidFinishLoad(ASender: TObject);

    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    { Public declarations }
  end;


var
  GlobalMobileAuthFrame:TFrameMobileAuth;



implementation

uses
  MobileMainForm;

{$R *.fmx}

{ TFrameMobileAuth }

procedure TFrameMobileAuth.btnAuthClick(Sender: TObject);
var
  APIItem_oauth2_authorize:TAPIItem_oauth2_authorize;
begin

  //授权
  APIItem_oauth2_authorize:=TAPIItem_oauth2_authorize.Create;
  APIItem_oauth2_authorize.ParamList.ClearValue;
  APIItem_oauth2_authorize.ParamList.SetValue('display','mobile');
  APIItem_oauth2_authorize.ParamList.SetValue('client_id',GlobalManager.App.AppKey);
  APIItem_oauth2_authorize.ParamList.SetValue('redirect_uri',GlobalManager.App.CallBackUrl);
  APIItem_oauth2_authorize.ParamList.SetValue('response_type','code');
  APIItem_oauth2_authorize.ParamList.SetValue('forcelogin','true');

  Self.FWebBrowser.Navigate(GlobalOpenPlatform.GetCallAPIUrl(GlobalManager.App,GlobalManager.OAuth2User,APIItem_oauth2_authorize));

end;

constructor TFrameMobileAuth.Create(AOwner: TComponent);
begin
  inherited;
  FWebBrowser:=TWebBrowserEx.Create(Self);
  FWebBrowser.Parent:=Self;
  FWebBrowser.Align:=TAlignLayout.Client;
  FWebBrowser.Visible:=True;
  FWebBrowser.OnDidFinishLoad:=Self.DoWebBrowserDidFinishLoad;
end;

destructor TFrameMobileAuth.Destroy;
begin
  FreeAndNil(FWebBrowser);
  inherited;
end;

procedure TFrameMobileAuth.DoWebBrowserDidFinishLoad(ASender: TObject);
var
  AUrlParamList:TUrlParamList;
  APIItem_oauth2_access_token:TAPIItem_oauth2_access_token;
  APIResponse_oauth2_access_token:TAPIResponse_oauth2_access_token;
begin
//  OutputDebugString(FWebBrowser.URL);
//  'https://api.weibo.com/oauth2/default.html?code=f94351c291d532fd87946fae69dde750'
  //判断是否是最终的提交页面
  if Copy(FWebBrowser.URL,1,Length(GlobalManager.App.CallBackUrl))=GlobalManager.App.CallBackUrl then
  begin
    //如果到的回调页面,那么解析返回的Code
    AUrlParamList:=ParseUrlParamList(FWebBrowser.URL,True);
    try
      if AUrlParamList.ItemByName['code']<>nil then
      begin


        //让用户授权,获取授权码
        //调用接口获取AccessToken
        APIItem_oauth2_access_token:=TAPIItem_oauth2_access_token.Create;
        try
          APIItem_oauth2_access_token.ParamList.SetValue('client_id',GlobalManager.App.AppKey);
          APIItem_oauth2_access_token.ParamList.SetValue('client_secret',GlobalManager.App.AppSecret);
          APIItem_oauth2_access_token.ParamList.SetValue('grant_type','authorization_code');

          APIItem_oauth2_access_token.ParamList.SetValue('code',AUrlParamList.ItemByName['code'].Value);
          APIItem_oauth2_access_token.ParamList.SetValue('redirect_uri',GlobalManager.App.CallBackUrl);

          APIResponse_oauth2_access_token:=TAPIResponse_oauth2_access_token.Create;
          try
            if GlobalOpenPlatform.CallAPI(GlobalManager.App,GlobalManager.OAuth2User,APIItem_oauth2_access_token,GlobalHttpControl,APIResponse_oauth2_access_token) then
            begin
              GlobalManager.OAuth2User.Uid:=APIResponse_oauth2_access_token.uid;
              GlobalManager.OAuth2User.AccessToken:=APIResponse_oauth2_access_token.access_token;
              GlobalManager.OAuth2User.Expires_In:=APIResponse_oauth2_access_token.expires_in;
              GlobalManager.OAuth2User.Remind_In:=APIResponse_oauth2_access_token.remind_in;
              GlobalManager.OAuth2User.LastAuthTime:=Now;
              //保存到配置文件中
              GlobalManager.OAuth2User.SaveToINI(uFileCommon.GetApplicationPath+'OAuth2User.ini');
              HideFrame(Self);

              frmMobileMain.ShowMainFrame;
            end
            else
            begin
              ShowMessage('获取授权会话失败!');
            end;
          finally
            FreeAndNil(APIResponse_oauth2_access_token);
          end;
        finally
          FreeAndNil(APIItem_oauth2_access_token);
        end;
      end
      else
      begin

      end;
    finally
      FreeAndNil(AUrlParamList);
    end;
  end;



end;

end.
