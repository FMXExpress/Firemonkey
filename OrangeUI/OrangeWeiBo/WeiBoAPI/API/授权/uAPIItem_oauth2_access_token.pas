//http://open.weibo.com/wiki/Oauth2/access_token
unit uAPIItem_oauth2_access_token;

interface

uses
  Classes,
  uOpenPlatform;

type
  TAPIResponse_oauth2_access_token=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    access_token:string;//	用于调用access_token，接口获取授权后的access token。
    expires_in:string;//	access_token的生命周期，单位是秒数。
    remind_in:string;//	access_token的生命周期（该参数即将废弃，开发者请使用expires_in）。
    uid:string;//	当前授权用户的UID。
  end;

  TAPIItem_oauth2_access_token=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_oauth2_access_token }

procedure TAPIItem_oauth2_access_token.Init;
begin
  inherited;
  Name:='oauth2/access_token';
  Descrip:='OAuth2的access_token接口';
  Url:='https://api.weibo.com/oauth2/access_token';
  HttpRequestMethods:=[hrmPost];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;

  Self.ParamList.Add('client_id',true,'string','申请应用时分配的AppKey。');
  Self.ParamList.Add('client_secret',true,'string','申请应用时分配的AppSecret。');
  Self.ParamList.Add('grant_type',true,'string','请求的类型，可以为authorization_code、password、refresh_token。');

  Self.ParamList.Add('code',true,'string','调用authorize获得的code值。');
  Self.ParamList.Add('redirect_uri',true,'string','回调地址，需需与注册应用里的回调地址一致。');

  Self.ParamList.Add('username',true,'string','授权用户的用户名。');
  Self.ParamList.Add('password',true,'string','授权用户的密码。');

end;

{ TAPIResponse_oauth2_access_token }

function TAPIResponse_oauth2_access_token.ParseDataStructure: Boolean;
begin
  Result:=False;

  if RootJson<>nil then
  begin
    access_token:=RootJson.S['access_token'];
    expires_in:=GetJsonStringValue(RootJson,'expires_in');
    remind_in:=GetJsonStringValue(RootJson,'remind_in');
    uid:=RootJson.S['uid'];
    Result:=True;
  end;

end;

initialization
  RegisterAPIItem('oauth2/access_token',TAPIItem_oauth2_access_token);


end.
