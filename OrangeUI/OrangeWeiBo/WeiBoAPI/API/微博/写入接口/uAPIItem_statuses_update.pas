//http://open.weibo.com/wiki/2/statuses/update
unit uAPIItem_statuses_update;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uFuncCommon,
  uDataStructure,
  uOpenPlatform;

type
  TAPIResponse_statuses_update=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    status:Tstatus;
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_statuses_update=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_statuses_update }

procedure TAPIItem_statuses_update.Init;
begin
  inherited;
  Name:='statuses/update';
  Descrip:='发布一条新微博';
  Url:='https://api.weibo.com/2/statuses/update.json';
  HttpRequestMethods:=[hrmPost];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。  ');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。  ');
  Self.ParamList.Add('status',true,'string','要发布的微博文本内容，必须做URLencode，内容不超过140个汉字。  ').NeedUrlEncord:=True;
  Self.ParamList.Add('visible',false,'int','微博的可见性，0：所有人能看，1：仅自己可见，2：密友可见，3：指定分组可见，默认为0。  ');
  Self.ParamList.Add('list_id',false,'string','微博的保护投递指定分组ID，只有当visible参数为3时生效且必选。  ');
  Self.ParamList.Add('lat',false,'float','纬度，有效范围：-90.0到+90.0，+表示北纬，默认为0.0。  ');
  Self.ParamList.Add('long',false,'float','经度，有效范围：-180.0到+180.0，+表示东经，默认为0.0。  ');
  Self.ParamList.Add('annotations',false,'string','元数据，主要是为了方便第三方应用记录一些适合于自己使用的信息，每条微博可以包含一个或者多个元数据，必须以json字串的形式提交，字串长度不超过512个字符，具体内容可以自定。  ');
  Self.ParamList.Add('rip',false,'string','开发者上报的操作用户真实IP，形如：211.156.0.1。  ');

  //连续两次发布的微博不可以重复；
  //非会员发表定向微博，分组成员数最多200。
end;

{ TAPIResponse_statuses_update }

constructor TAPIResponse_statuses_update.Create;
begin
end;

destructor TAPIResponse_statuses_update.Destroy;
begin
  FreeAndNil(status);
  inherited;
end;

function TAPIResponse_statuses_update.ParseDataStructure: Boolean;
begin
  Result:=False;

  FreeAndNil(status);

  if RootJson<>nil then
  begin
    status:=Tstatus.Create;
    status.ParseFromJson(RootJson);
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('statuses/update',TAPIItem_statuses_update);

end.
