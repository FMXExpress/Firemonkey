//http://open.weibo.com/wiki/Oauth2/authorize
unit uAPIItem_oauth2_authorize;

interface

uses
  Classes,
  uOpenPlatform;

type
  TAPIItem_oauth2_authorize=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_oauth2_authorize }

procedure TAPIItem_oauth2_authorize.Init;
begin
  inherited;
  Name:='oauth2/authorize';
  Descrip:='OAuth2的authorize接口';
  Url:='https://api.weibo.com/oauth2/authorize';
  HttpRequestMethods:=[hrmGet,hrmPost];
  ResponseDataType:=rdtRedirectUrl;
  ResponseDataFormat:=rdfUrl;

  Self.ParamList.Add('client_id',true,'string','申请应用时分配的AppKey。');
  Self.ParamList.Add('redirect_uri',true,'string','授权回调地址，站外应用需与设置的回调地址一致，站内应用需填写canvas page的地址。');
  Self.ParamList.Add('scope',false,'string','申请scope权限所需参数，可一次申请多个scope权限，用逗号分隔。使用文档');
  Self.ParamList.Add('state',false,'string','用于保持请求和回调的状态，在回调时，会在Query Parameter中回传该参数。开发者可以用这个参数验证请求有效性，也可以记录用户请求授权页前的位置。这个参数可用于防止跨站请求伪造（CSRF）攻击');
  Self.ParamList.Add('display',false,'string','授权页面的终端类型，取值见下面的说明。');
  Self.ParamList.Add('forcelogin',false,'boolean','是否强制用户重新登录，true：是，false：否。默认false。');
  Self.ParamList.Add('language',false,'string','授权页语言，缺省为中文简体版，en为英文版。英文版测试中，开发者任何意见可反馈至 @微博API');
  Self.ParamList.Add('response_type',true,'string','');


  Self.ParamList.ItemByName['display'].ValueScoptList.Add('default','默认的授权页面，适用于web浏览器。');
  Self.ParamList.ItemByName['display'].ValueScoptList.Add('mobile','移动终端的授权页面，适用于支持html5的手机。');
  Self.ParamList.ItemByName['display'].ValueScoptList.Add('wap1.2','默认的授权页面，wap1.2的授权页面。');
  Self.ParamList.ItemByName['display'].ValueScoptList.Add('wap2.0','wap2.0的授权页面。');
  Self.ParamList.ItemByName['display'].ValueScoptList.Add('apponweibo','默认的站内应用授权页，授权后不返回access_token，只刷新站内应用父框架。');

//default  默认的授权页面，适用于web浏览器。
//mobile  移动终端的授权页面，适用于支持html5的手机。注：使用此版授权页请用 https://open.weibo.cn/oauth2/authorize 授权接口
//wap  wap版授权页面，适用于非智能手机。
//client  客户端版本授权页面，适用于PC桌面应用。
//apponweibo  默认的站内应用授权页，授权后不返回access_token，只刷新站内应用父框架。


end;

initialization
  RegisterAPIItem('oauth2/authorize',TAPIItem_oauth2_authorize);

end.
