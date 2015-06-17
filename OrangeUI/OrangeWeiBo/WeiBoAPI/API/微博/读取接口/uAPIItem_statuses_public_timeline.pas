//http://open.weibo.com/wiki/2/statuses/public_timeline
unit uAPIItem_statuses_public_timeline;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uDataStructure,
  uFuncCommon,
  uOpenPlatform;

type
  TAPIResponse_statuses_public_timeline=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    statuses:Tstatuses;
//    "previous_cursor": 0,                    // 暂未支持
//    "next_cursor": 11488013766,     // 暂未支持
//    "total_number": 81655

    previous_cursor:int;//": 0,                    // 暂未支持
    next_cursor:int;//": 11488013766,     // 暂未支持
    total_number:int;//": 81655

    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_statuses_public_timeline=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_statuses_public_timeline }

procedure TAPIItem_statuses_public_timeline.Init;
begin
  inherited;
  Name:='statuses/public_timeline';
  Descrip:='返回最新的200条公共微博，返回结果非完全实时';
  Url:='https://api.weibo.com/2/statuses/public_timeline.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。');
  Self.ParamList.Add('count',false,'int','单页返回的记录条数，最大不超过200，默认为20。');


end;

{ TAPIResponse_statuses_public_timeline }

constructor TAPIResponse_statuses_public_timeline.Create;
begin
  statuses:=Tstatuses.Create;
end;

destructor TAPIResponse_statuses_public_timeline.Destroy;
begin
  FreeAndNil(statuses);
  inherited;
end;

function TAPIResponse_statuses_public_timeline.ParseDataStructure: Boolean;
var
  I: Integer;
  Astatus:Tstatus;
  AstatusJson:ISuperObject;
  AstatusesJson:ISuperObject;
begin
  Result:=False;

  statuses.Clear(True);

  if RootJson<>nil then
  begin
    AstatusesJson:=RootJson.O['statuses'];
    if AstatusesJson<>nil then
    begin
//      for I := 0 to AstatusesJson.AsArray.Length - 1 do
//      begin
//        AstatusJson:=AstatusesJson.AsArray[I];
//        if AstatusJson<>nil then
//        begin
//          Astatus:=Tstatus.Create;
//          Astatus.ParseFromJson(AstatusJson);
//          statuses.Add(Astatus);
//        end;
//      end;
    end;

    previous_cursor:=RootJson.I['previous_cursor'];
    next_cursor:=RootJson.I['next_cursor'];
    total_number:=RootJson.I['total_number'];
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('statuses/public_timeline',TAPIItem_statuses_public_timeline);

end.
