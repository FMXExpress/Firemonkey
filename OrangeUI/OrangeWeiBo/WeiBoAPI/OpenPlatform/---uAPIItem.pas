unit uAPIItem;

interface

uses
//  Windows,
  uBaseList;

type
  //支持的HTTP请求方法
  THTTPRequestMethod=(hrmGet,hrmPost);
  THTTPRequestMethods=set of THTTPRequestMethod;
  //返回数据格式
  TReponseDataFormat=(rdfUrl,rdfJson,rdfXml);
  //接口需要的访问级别
  TAccessLevel=(alNormal);
  //返回数据类型
  TResponseDataType=(rdtRedirectUrl,rdtData);

  TValueScope=class
  public
    //参数取值
    Value:Variant;
    //类型说明
    Descrip:String;
  end;

  TValueScopeList=class(TBaseList)
  private
    function GetItem(Index: Integer): TValueScope;
  public
    function Add(const Value:Variant;const Descrip:String):TValueScope;
    property Items[Index:Integer]:TValueScope read GetItem;
  end;

  TAPIParam=class
  public
    //参数名
    Name:String;
    //必选
    Need:Boolean;
    //类型及范围
    Kind:String;
    //说明
    Descrip:String;
    //参数值
    Value:Variant;
    //参数范围
    ValueScoptList:TValueScopeList;
  public
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIParamList=class(TBaseList)
  private
    function GetItem(Index: Integer): TAPIParam;
    function GetItemByName(const Name: String): TAPIParam;
  public
    //清除API参数列表的值
    procedure ClearValue;
    property Items[Index:Integer]:TAPIParam read GetItem;
    property ItemByName[const Name:String]:TAPIParam read GetItemByName;
    //设置值
    function Add( const Name:String;
                  const Need:Boolean;
                  const Kind:String;
                  const Descrip:String
                  ):TAPIParam;
  end;

  TAPIItem=class
  public
    //API名称
    Name:String;
    //介绍
    Descrip:String;
    //URL
    URL:String;
    //返回数据类型
    ResponseDataType:TResponseDataType;
    //返回数据格式
    ResponseDataFormat:TReponseDataFormat;
    //HTTP请求方式
    HttpRequestMethods:THTTPRequestMethods;
    //是否需要登录
    NeedLogin:Boolean;
    //访问授权限制
    //访问级别
    AccessLevel:TAccessLevel;
    //频次限制
    TimesLimit:Boolean;
    //参数列表
    ParamList:TAPIParamList;
  protected
    procedure Init;virtual;abstract;
  public
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIList=class(TBaseList)
  private
    function GetItem(Index: Integer): TAPIItem;
  public
    property Items[Index:Integer]:TAPIItem read GetItem;
  end;

implementation

{ TValueScopeList }

function TValueScopeList.Add(const Value: Variant;
                              const Descrip: String): TValueScope;
begin
  Result:=TValueScope.Create;
  Result.Value:=Value;
  Result.Descrip:=Descrip;
end;

function TValueScopeList.GetItem(Index: Integer): TValueScope;
begin
  Result:=TValueScope(Inherited Items[Index]);
end;

{ TAPIParam }

constructor TAPIParam.Create;
begin
  ValueScoptList:=TValueScopeList.Create;
end;

destructor TAPIParam.Destroy;
begin
  ValueScoptList.Clear(True);
  ValueScoptList.Free;
  inherited;
end;

{ TAPIItem }

constructor TAPIItem.Create;
begin
  ParamList:=TAPIParamList.Create;
end;

destructor TAPIItem.Destroy;
begin
  ParamList.Clear(True);
  ParamList.Free;
  inherited;
end;

{ TAPIParamList }

function TAPIParamList.Add(const Name: String;
                            const Need: Boolean;
                            const Kind: String;
                            const Descrip: String): TAPIParam;
begin
  Result:=TAPIParam.Create;
  Result.Name:=Name;
  Result.Need:=Need;
  Result.Kind:=Kind;
  Result.Descrip:=Descrip;
end;

procedure TAPIParamList.ClearValue;
var
  I:Integer;
begin
  For I:=0 to Self.Count-1 do
  begin
    //varEmpty或varNUll
    Items[I].Value:=varNUll;
  end;
end;

function TAPIParamList.GetItem(Index: Integer): TAPIParam;
begin
  Result:=TAPIParam(Inherited Items[Index]);
end;

function TAPIParamList.GetItemByName(const Name: String): TAPIParam;
var
  I:Integer;
begin
  Result:=nil;
  For I:=0 to Self.Count-1 do
  begin
    if Items[I].Name=Name then
    begin
      Result:=Items[I];
    end;
  end;
end;

{ TAPIList }

function TAPIList.GetItem(Index: Integer): TAPIItem;
begin
  Result:=TAPIItem(inherited Items[Index]);
end;

end.


