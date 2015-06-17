//http://open.weibo.com/wiki/2/users/counts
unit uAPIItem_users_counts;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uDataStructure,
  uFuncCommon,
  uOpenPlatform;

type
  TAPIResponse_users_counts=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    user_counts:Tuser_counts;
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_users_counts=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_users_counts }

procedure TAPIItem_users_counts.Init;
begin
  inherited;
  Name:='users/counts';
  Descrip:='根据微博ID返回某条微博的评论列表';
  Url:='https://api.weibo.com/2/users/counts.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。  ');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。  ');
  Self.ParamList.Add('uids',true,'string','需要获取数据的用户UID，多个之间用逗号分隔，最多不超过100个。 ');


end;

{ TAPIResponse_users_counts }

constructor TAPIResponse_users_counts.Create;
begin
  user_counts:=Tuser_counts.Create;
end;

destructor TAPIResponse_users_counts.Destroy;
begin
  FreeAndNil(user_counts);
  inherited;
end;

function TAPIResponse_users_counts.ParseDataStructure: Boolean;
var
  I: Integer;
  Auser_count:Tuser_count;
  Auser_countJson:ISuperObject;
  Auser_countsJson:ISuperArray;
//  ACast:TCast;
begin
  Result:=False;

  user_counts.Clear(True);

  if RootJson<>nil then
  begin
//    ACast:=TCast.Create(RootJson);
//    Auser_countsJson:=ACast.AsArray;
//    if Auser_countsJson<>nil then
//    begin
//      for I := 0 to Auser_countsJson.Length - 1 do
//      begin
//        Auser_countJson:=Auser_countsJson[I];
//        if Auser_countJson<>nil then
//        begin
//          Auser_count:=Tuser_count.Create;
//          Auser_count.ParseFromJson(Auser_countJson);
//          user_counts.Add(Auser_count);
//        end;
//      end;
//    end;
//    ACast.Free;
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('users/counts',TAPIItem_users_counts);

end.
