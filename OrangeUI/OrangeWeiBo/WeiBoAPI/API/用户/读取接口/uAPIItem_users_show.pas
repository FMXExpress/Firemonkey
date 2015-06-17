//http://open.weibo.com/wiki/2/users/show
unit uAPIItem_users_show;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uDataStructure,
  uFuncCommon,
  uOpenPlatform;

type
  TAPIResponse_users_show=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    user:Tuser;
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_users_show=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_users_show }

procedure TAPIItem_users_show.Init;
begin
  inherited;
  Name:='users/show';
  Descrip:='获取用户信息';
  Url:='https://api.weibo.com/2/users/show.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。  ');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。  ');
  Self.ParamList.Add('uid',false,'int64','需要查询的用户ID。  ');
  Self.ParamList.Add('screen_name',false,'string','需要查询的用户昵称。  ');


end;

{ TAPIResponse_users_show }

constructor TAPIResponse_users_show.Create;
begin
end;

destructor TAPIResponse_users_show.Destroy;
begin
  FreeAndNil(user);
  inherited;
end;

function TAPIResponse_users_show.ParseDataStructure: Boolean;
begin
  Result:=False;

  if RootJson<>nil then
  begin
    user:=Tuser.Create;
    user.ParseFromJson(RootJson);
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('users/show',TAPIItem_users_show);

end.
