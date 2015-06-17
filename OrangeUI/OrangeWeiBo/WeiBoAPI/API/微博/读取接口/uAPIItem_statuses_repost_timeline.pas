//http://open.weibo.com/wiki/2/statuses/repost_timeline
unit uAPIItem_statuses_repost_timeline;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uFuncCommon,
  uDataStructure,
  uOpenPlatform;

type
  TAPIResponse_statuses_repost_timeline=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    reposts:Tstatuses;
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

  TAPIItem_statuses_repost_timeline=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_statuses_repost_timeline }

procedure TAPIItem_statuses_repost_timeline.Init;
begin
  inherited;
  Name:='statuses/repost_timeline';
  Descrip:='获取指定微博的转发微博列表';
  Url:='https://api.weibo.com/2/statuses/repost_timeline.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。  ');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。  ');
  Self.ParamList.Add('id',true,'int64','需要查询的微博ID。  ');
  Self.ParamList.Add('since_id',false,'int64','若指定此参数，则返回ID比since_id大的微博（即比since_id时间晚的微博），默认为0。  ');
  Self.ParamList.Add('max_id',false,'int64','若指定此参数，则返回ID小于或等于max_id的微博，默认为0。  ');
  Self.ParamList.Add('count',false,'int','单页返回的记录条数，最大不超过200，默认为20。  ');
  Self.ParamList.Add('page',false,'int','返回结果的页码，默认为1。  ');
  Self.ParamList.Add('filter_by_author',false,'int','作者筛选类型，0：全部、1：我关注的人、2：陌生人，默认为0。  ');

end;

{ TAPIResponse_statuses_repost_timeline }

constructor TAPIResponse_statuses_repost_timeline.Create;
begin
  reposts:=Tstatuses.Create;
end;

destructor TAPIResponse_statuses_repost_timeline.Destroy;
begin
  FreeAndNil(reposts);
  inherited;
end;

function TAPIResponse_statuses_repost_timeline.ParseDataStructure: Boolean;
var
  I: Integer;
  Astatus:Tstatus;
  AstatusJson:ISuperObject;
  AstatusesJson:ISuperObject;
begin
  Result:=False;

  reposts.Clear(True);

  if RootJson<>nil then
  begin
    AstatusesJson:=RootJson.O['reposts'];
    if AstatusesJson<>nil then
    begin
//      for I := 0 to AstatusesJson.AsArray.Length - 1 do
//      begin
//        AstatusJson:=AstatusesJson.AsArray[I];
//        if AstatusJson<>nil then
//        begin
//          Astatus:=Tstatus.Create;
//          Astatus.ParseFromJson(AstatusJson);
//          reposts.Add(Astatus);
//        end;
//      end;
    end;

//    hasvisible:=RootJson.B['hasvisible'];//":false,
    previous_cursor:=RootJson.I['previous_cursor'];//":0,
    next_cursor:=RootJson.I['next_cursor'];//":3669625160565342,
    total_number:=RootJson.I['total_number'];//":2020
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('statuses/repost_timeline',TAPIItem_statuses_repost_timeline);

end.
