//http://open.weibo.com/wiki/2/statuses/home_timeline
unit uAPIItem_statuses_home_timeline;

interface
//{$I FrameWork.inc}

{$DEFINE FMX}

uses
  Classes,
  SysUtils,
  XSuperObject,
  uFuncCommon,
  uDataStructure,
  uOpenPlatform;

type
  TAPIResponse_statuses_home_timeline=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    statuses:Tstatuses;
//    "hasvisible":false,
//    "previous_cursor":0,
//    "next_cursor":3669625160565342,
//    "total_number":2020
//    hasvisible:boolean;//":false,
    previous_cursor:int;//":0,
    next_cursor:int;//":3669625160565342,
    total_number:int;//":2020
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_statuses_home_timeline=class(TAPIItem)
  protected
    procedure Init;override;
  end;
//{$I FrameWork.inc}

implementation

{ TAPIItem_statuses_home_timeline }

procedure TAPIItem_statuses_home_timeline.Init;
begin
  inherited;
  Name:='statuses/home_timeline';
  Descrip:='获取当前登录用户及其所关注用户的最新微博';
  Url:='https://api.weibo.com/2/statuses/home_timeline.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。');
  Self.ParamList.Add('since_id',false,'int64','若指定此参数，则返回ID比since_id大的微博（即比since_id时间晚的微博），默认为0。');
  Self.ParamList.Add('max_id',false,'int64','若指定此参数，则返回ID小于或等于max_id的微博，默认为0。');
  Self.ParamList.Add('count',false,'int','单页返回的记录条数，最大不超过100，默认为20。');
  Self.ParamList.Add('page',false,'int','返回结果的页码，默认为1。');
  Self.ParamList.Add('base_app',false,'int','是否只获取当前应用的数据。0为否（所有数据），1为是（仅当前应用），默认为0。');
  Self.ParamList.Add('feature',false,'int','过滤类型ID，0：全部、1：原创、2：图片、3：视频、4：音乐，默认为0。');
  Self.ParamList.Add('trim_user',false,'int','返回值中user字段开关，0：返回完整user字段、1：user字段仅返回user_id，默认为0。');


end;

{ TAPIResponse_statuses_home_timeline }

constructor TAPIResponse_statuses_home_timeline.Create;
begin
  statuses:=Tstatuses.Create;
end;

destructor TAPIResponse_statuses_home_timeline.Destroy;
begin
  FreeAndNil(statuses);
  inherited;
end;

function TAPIResponse_statuses_home_timeline.ParseDataStructure: Boolean;
var
  I: Integer;
  Astatus:Tstatus;
  AstatusJson:ISuperObject;
  AstatusesJson:ISuperArray;
begin
  Result:=False;

  statuses.Clear(True);

  if (RootJson<>nil) and RootJson.Contains('statuses') then
  begin

//    {$IFDEF VCL}
//    AstatusesJson:=RootJson.A['statuses'];
//    if AstatusesJson<>nil then
//    begin
//      for I := 0 to AstatusesJson.Length - 1 do
//      begin
//        AstatusJson:=AstatusesJson[I];
//        if AstatusJson<>nil then
//        begin
//          Astatus:=Tstatus.Create;
//          Astatus.ParseFromJson(AstatusJson);
//          statuses.Add(Astatus);
//        end;
//      end;
//    end;
//    {$ENDIF}
//
//    {$IFDEF FMX}
    AstatusesJson:=RootJson.A['statuses'];
    if AstatusesJson<>nil then
    begin
      for I := 0 to AstatusesJson.Length - 1 do
      begin
        AstatusJson:=AstatusesJson.O[I];
//        AstatusJson:=RootJson.O['"statuses"[0]'];
        if AstatusJson<>nil then
        begin
          Astatus:=Tstatus.Create;
          Astatus.ParseFromJson(AstatusJson);
          statuses.Add(Astatus);
        end;
      end;
    end;
//    {$ENDIF}



//    hasvisible:=RootJson.B['hasvisible'];//":false,
    previous_cursor:=RootJson.I['previous_cursor'];//":0,
    next_cursor:=RootJson.I['next_cursor'];//":3669625160565342,
    total_number:=RootJson.I['total_number'];//":2020
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('statuses/home_timeline',TAPIItem_statuses_home_timeline);

end.
