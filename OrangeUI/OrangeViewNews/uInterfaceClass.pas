unit uInterfaceClass;

interface

uses
  Classes,
  SysUtils,
  uFuncCommon,
  Variants,
  DateUtils,
  uPublic,
  uBaseLog,
  XSuperObject,
//  uUrlParam,
  INIFiles,
  uBaseList
  ;

type
  //支持的HTTP请求方法
  THTTPRequestMethod=(hrmGet,hrmPost);
  THTTPRequestMethods=set of THTTPRequestMethod;
  //返回数据类型
  TResponseDataType=(rdtJson,rdtHtml);

  TInterfaceUrlParam=class
  public
    //参数名
    Name:String;
    //说明
    Descrip:String;
    //参数值
    Value:Variant;
  public
    constructor Create;
    destructor Destroy;override;
  end;

  TInterfaceUrlParamList=class(TBaseList)
  private
    function GetItem(Index: Integer): TInterfaceUrlParam;
    function GetItemByName(const Name: String): TInterfaceUrlParam;
  public
    //清除Interface参数列表的值
    procedure ClearValue;
    function GetUrlParamListStr:String;
    procedure SetValue(const Name:String;const Value:Variant);
    property Items[Index:Integer]:TInterfaceUrlParam read GetItem;default;
    property ItemByName[const Name:String]:TInterfaceUrlParam read GetItemByName;
    //设置值
    function Add( const Name:String;
                  const Descrip:String
                  ):TInterfaceUrlParam;
  end;



  TInterfaceSimpleJsonParam=class
  public
    //参数名
    Name:String;
    //说明
    Descrip:String;
    //参数值
    Value:Variant;
  public
    constructor Create;
    destructor Destroy;override;
  end;

  TInterfaceSimpleJsonParamList=class(TBaseList)
  private
    function GetItem(Index: Integer): TInterfaceSimpleJsonParam;
    function GetItemByName(const Name: String): TInterfaceSimpleJsonParam;
  public
    //清除Interface参数列表的值
    procedure ClearValue;
    procedure SetValue(const Name:String;const Value:Variant);
    property Items[Index:Integer]:TInterfaceSimpleJsonParam read GetItem;default;
    property ItemByName[const Name:String]:TInterfaceSimpleJsonParam read GetItemByName;
    //设置值
    function Add( const Name:String;
                  const Descrip:String
                  ):TInterfaceSimpleJsonParam;
  end;







  TInterfaceItem=class(TObject)
  public
//    //Interface名称
//    Name:String;
    //URL
    URL:String;
    IsWholeURL:Boolean;

    //介绍
    Descrip:String;

    //返回数据类型
    ResponseDataType:TResponseDataType;
//    //返回数据格式
//    ResponseDataFormat:TReponseDataFormat;
    //HTTP请求方式
    HttpRequestMethods:THTTPRequestMethods;
    //Url参数列表
    UrlParamList:TInterfaceUrlParamList;
    //
    SimpleJsonParamList:TInterfaceSimpleJsonParamList;
  private
    FRequestJson: ISuperObject;
    function GetRequestJson: ISuperObject;
    function ParseFromStream(AResponseStream:TStream):Boolean;
  protected
    //解析数据
    function ParseResponseData:Boolean;virtual;abstract;
  public
    CallResult:Boolean;
    ResponseJson:ISuperObject;
    ResponseStream:TMemoryStream;
    property RequestJson:ISuperObject Read GetRequestJson;
    //生成请求数据
    function GenerateRequestStream:TStream;virtual;
  public
    constructor Create;virtual;
    procedure AfterConstruction;override;
    destructor Destroy;override;
  end;




  TInterfaceSetting=class
  public
    UserId:String;
    Password:String;
    Host:String;
  public
    constructor Create;
  end;

  THttpControl=class
//  private
//    //临界区
//    FHttpLock:TRTLCriticalSection;
  public
    procedure Lock;
    procedure UnLock;
    constructor Create;virtual;
    destructor Destroy;override;
    function Get(const HttpUrl:String;AResponseStream:TStream):Boolean;overload;virtual;abstract;
    function Get(const HttpUrl:String):String;overload;virtual;abstract;
    function Post(const HttpUrl:String;ARequestStream:TStream;AResponseStream:TStream):Boolean;overload;virtual;abstract;
    function Post(const HttpUrl:String;ARequestStream:TStream):String;overload;virtual;abstract;
  end;



//获取Interface的请求Url
function GetCallInterfaceUrl(InterfaceSetting:TInterfaceSetting;
                              InterfaceItem:TInterfaceItem):String;
//调用Interface
function CallInterface(InterfaceSetting:TInterfaceSetting;
                      InterfaceItem:TInterfaceItem;
                      HttpControl:THttpControl;
                      AUrlParamNames:Array of String;
                      AUrlParamValues:Array of Variant;
                      ASimpleJsonParamNames:Array of String;
                      ASimpleJsonParamValues:Array of Variant;
                      AResponseStream:TStream):Boolean;overload;
function CallInterface(InterfaceSetting:TInterfaceSetting;
                      InterfaceItem:TInterfaceItem;
                      HttpControl:THttpControl;
                      AUrlParamNames:Array of String;
                      AUrlParamValues:Array of Variant;
                      ASimpleJsonParamNames:Array of String;
                      ASimpleJsonParamValues:Array of Variant):Boolean;overload;

var
  GlobalInterfaceSetting:TInterfaceSetting;

implementation


{ TOpenPlatform }

function CallInterface(InterfaceSetting: TInterfaceSetting;
                      InterfaceItem: TInterfaceItem;
                      HttpControl: THttpControl;
                      AUrlParamNames:Array of String;
                      AUrlParamValues:Array of Variant;
                      ASimpleJsonParamNames:Array of String;
                      ASimpleJsonParamValues:Array of Variant;
                      AResponseStream: TStream): Boolean;
var
  I:Integer;
  HttpUrl:String;
  ARequestStream:TStream;
begin
  Result:=False;
  InterfaceItem.CallResult:=False;

  for I:=0 to Length(AUrlParamNames)-1 do
  begin
    InterfaceItem.UrlParamList.SetValue(AUrlParamNames[I],AUrlParamValues[I]);
  end;


  HttpUrl:=GetCallInterfaceUrl(InterfaceSetting,InterfaceItem);



  try

    if hrmGet in InterfaceItem.HttpRequestMethods then
    begin
      InterfaceItem.CallResult:=HttpControl.Get(HttpUrl,AResponseStream);
    end
    else
    begin

  //    for I:=0 to Length(ASimpleJsonParamNames)-1 do
  //    begin
  //      InterfaceItem.SimpleJsonParamList.SetValue(ASimpleJsonParamNames[I],ASimpleJsonParamValues[I]);
  //    end;


  //    ARequestStream:=TMemoryStream.Create;
      ARequestStream:=InterfaceItem.GenerateRequestStream;
      try
        ARequestStream.Position:=0;
        InterfaceItem.CallResult:=HttpControl.Post(HttpUrl,ARequestStream,AResponseStream);
      finally
        FreeAndNil(ARequestStream);
      end;
    end;

  except
    on E:Exception do
    begin
      uBaseLog.HandleException(E,'Main','uInterfaceClass','CallInterface',HttpUrl);
    end;
  end;



  Result:=InterfaceItem.CallResult;

end;

function CallInterface(InterfaceSetting:TInterfaceSetting;
                      InterfaceItem:TInterfaceItem;
                      HttpControl:THttpControl;
                      AUrlParamNames:Array of String;
                      AUrlParamValues:Array of Variant;
                      ASimpleJsonParamNames:Array of String;
                      ASimpleJsonParamValues:Array of Variant):Boolean;overload;
begin
  Result:=False;
  if InterfaceItem.ResponseStream=nil then
  begin
    InterfaceItem.ResponseStream:=TMemoryStream.Create;
  end;
  InterfaceItem.ResponseStream.Clear;

  if CallInterface(InterfaceSetting,
                        InterfaceItem,
                        HttpControl,
                        AUrlParamNames,
                        AUrlParamValues,
                        ASimpleJsonParamNames,
                        ASimpleJsonParamValues,
                        InterfaceItem.ResponseStream) then
  begin
    case InterfaceItem.ResponseDataType of
      rdtJson:
      begin
        InterfaceItem.ResponseStream.
            SaveToFile(GetResponseTempDir+InterfaceItem.Descrip+'_'+FormatDateTime('YYYY-MM-DD HH-MM-SS-ZZZ',Now)+'.json');
//        uBaseLog.OutputDebugString('SaveToFile');
//        if InterfaceItem=nil then
//        begin
//          uBaseLog.OutputDebugString('InterfaceItem=nil');
//        end;
        InterfaceItem.ResponseStream.Position:=0;
//          uBaseLog.OutputDebugString('InterfaceItem=nil');
        Result:=InterfaceItem.ParseFromStream(InterfaceItem.ResponseStream);
//        uBaseLog.OutputDebugString('ParseFromStream');
      end;
      rdtHtml:
      begin
        TMemoryStream(InterfaceItem.ResponseStream).
            SaveToFile(GetResponseTempDir+InterfaceItem.Descrip+'_'+FormatDateTime('YYYY-MM-DD HH-MM-SS-ZZZ',Now)+'.html');
        InterfaceItem.ResponseStream.Position:=0;
        Result:=True;
      end;
    end;
  end;
end;

function GetCallInterfaceUrl(InterfaceSetting: TInterfaceSetting;
                             InterfaceItem: TInterfaceItem): String;
//var
//  I: Integer;
//  AUrlParamList:TUrlParamList;
begin
  Result:='';
//  AUrlParamList:=TUrlParamList.Create;
//  Try
//    for I := 0 to InterfaceItem.UrlParamList.Count - 1 do
//    begin
//      if Not VarIsNull(InterfaceItem.UrlParamList[I].Value) then
//      begin
//        AUrlParamList.AddUrlParam(InterfaceItem.UrlParamList[I].Name,
//              InterfaceItem.UrlParamList[I].Value);
//      end;
//    end;
    if Not InterfaceItem.IsWholeURL then
    begin
      Result:='http://'
                  +InterfaceSetting.UserId
                  +':'
                  +InterfaceSetting.Password
                  +'@'
                  +InterfaceSetting.Host
                  +InterfaceItem.URL;
    end
    else
    begin
      Result:=InterfaceItem.URL;
    end;
//    if InterfaceItem.UrlParamList.GetUrlParamListStr<>'' then
//    begin
      Result:=Result+InterfaceItem.UrlParamList.GetUrlParamListStr;
//    end;
//    if AUrlParamList.GetUrlParamListStr<>'' then
//    begin
//      Result:=Result+AUrlParamList.GetUrlParamListStr;
//    end;
//
//  Finally
//    AUrlParamList.Clear(True);
//    AUrlParamList.Free;
//  End;
end;

{ TInterfaceUrlParam }

constructor TInterfaceUrlParam.Create;
begin
end;

destructor TInterfaceUrlParam.Destroy;
begin
  inherited;
end;

{ TInterfaceItem }

procedure TInterfaceItem.AfterConstruction;
begin
  inherited;
  Self.UrlParamList.Add('format','格式');

  Self.UrlParamList.Add('username','监管平台用户名');
  Self.UrlParamList.Add('from','来源, 1--手机端、2--速测仪、3--第三方平台');
  Self.UrlParamList.Add('region_code','区域码');

end;

constructor TInterfaceItem.Create;
begin
  IsWholeURL:=False;
  ResponseDataType:=rdtJson;
  UrlParamList:=TInterfaceUrlParamList.Create;
  SimpleJsonParamList:=TInterfaceSimpleJsonParamList.Create;



end;

destructor TInterfaceItem.Destroy;
begin
//        uBaseLog.OutputDebugString('TInterfaceItem.Destroy'+Self.Descrip);
//  FreeAndNil(ResponseJson);
  FreeAndNil(ResponseStream);
  UrlParamList.Clear(True);
  FreeAndNil(UrlParamList);
  SimpleJsonParamList.Clear(True);
  FreeAndNil(SimpleJsonParamList);
  inherited;
end;

function TInterfaceItem.GenerateRequestStream: TStream;
var
  I: Integer;
  AVarType:Word;
begin
  Result:=TStringStream.Create;
  for I := 0 to Self.SimpleJsonParamList.Count-1 do
  begin
    if Not VarIsNull(Self.SimpleJsonParamList.Items[I].Value) then
    begin
      AVarType:=VarType(Self.SimpleJsonParamList.Items[I].Value);
      case AVarType of
          varEmpty:;//    = $0000; { vt_empty        0 }
          varNull:;//     = $0001; { vt_null         1 }
          varSmallint:;// = $0002; { vt_i2           2 }
          varInteger:Self.RequestJson.I[Self.SimpleJsonParamList.Items[I].Name]:=Self.SimpleJsonParamList.Items[I].Value;//  = $0003; { vt_i4           3 }
          varSingle:;//   = $0004; { vt_r4           4 }
          varDouble:;//   = $0005; { vt_r8           5 }
          varCurrency:;// = $0006; { vt_cy           6 }
          varDate:;//     = $0007; { vt_date         7 }
          varOleStr:;//   = $0008; { vt_bstr         8 }
          varDispatch:;// = $0009; { vt_dispatch     9 }
          varError:;//    = $000A; { vt_error       10 }
          varBoolean:;//  = $000B; { vt_bool        11 }
          varVariant:;//  = $000C; { vt_variant     12 }
          varUnknown:;//  = $000D; { vt_unknown     13 }
        //varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
        //varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
          varShortInt:;// = $0010; { vt_i1          16 }
          varByte:;//     = $0011; { vt_ui1         17 }
          varWord:;//     = $0012; { vt_ui2         18 }
          varLongWord:;// = $0013; { vt_ui4         19 }
          varInt64:;//    = $0014; { vt_i8          20 }
          varUInt64:;//   = $0015; { vt_ui8         21 }
          varRecord:;//   = $0024; { VT_RECORD      36 }
        {  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

          varStrArg:;//   = $0048; { vt_clsid        72 }
          varObject:;//   = $0049; {                 73 }
          varUStrArg:;//  = $004A; {                 74 }
          varString:Self.RequestJson.S[Self.SimpleJsonParamList.Items[I].Name]:=Self.SimpleJsonParamList.Items[I].Value;//   = $0100; { Pascal string  256 } {not OLE compatible }
          varAny:;//      = $0101; { Corba any      257 } {not OLE compatible }
          varUString:Self.RequestJson.S[Self.SimpleJsonParamList.Items[I].Name]:=Self.SimpleJsonParamList.Items[I].Value;//  = $0102; { Unicode string 258 } {not OLE compatible }
          // custom types range from $110 (272) to $7FF (2047)

          varTypeMask:;// = $0FFF;
          varArray:;//    = $2000;
          varByRef:;//    = $4000;

      end;
    end;
  end;
//  RequestJson.AsJSON;
//  RequestJson.SaveTo(Result);
  TStringStream(Result).WriteString(RequestJson.AsJSON);
  Result.Position:=0;
end;

function TInterfaceItem.GetRequestJson: ISuperObject;
begin
  if FRequestJson=nil then
  begin
    FRequestJson:=TSuperObject.Create();
  end;
  Result:=FRequestJson;
end;

function TInterfaceItem.ParseFromStream(AResponseStream: TStream): Boolean;
var
  I:Integer;
  AString:String;
  AStingList:TStringList;
  AStringStream:TStringStream;
begin
  Result:=False;
  AResponseStream.Position:=0;


//  AStingList:=TStringList.Create;
//  AStingList.LoadFromStream(AResponseStream,TEncoding.UTF8);
//  AString:='';
//  for I := 0 to AStingList.Count - 1 do
//  begin
//    AString:=AString+AStingList[I]+#13#10;
//  end;
//    uBaseLog.OutputDebugString('AResponseStream');

  AResponseStream.Position:=0;
  ResponseJson:=TSuperObject.ParseStream(AResponseStream);

////  {$IFDEF FMX}
////  AResponseStream.Position:=0;
////  RootJson:=TSuperObject.ParseStream(AResponseStream);
//  AStringStream:=TStringStream.Create;
////  AStingList.SaveToStream(AStringStream);
//  AStringStream.WriteString(AString);
//  AStringStream.Position:=0;
//  ResponseJson:=TSuperObject.ParseStream(AStringStream);
//  AStringStream.Free;
////  {$ENDIF}


//  {$IFDEF VCL}
//  RootJson:=TSuperObject.ParseString(PWideChar(AString),True);
//  {$ENDIF}
  //不会自动识别编辑，郁闷
//  RootJson:=TSuperObject.ParseStream(AResponseStream,True);

  //判断有没有正确调用
  if (ResponseJson<>nil)
    and ResponseJson.Contains('status')
    and (ResponseJson.GetType('status')=varBoolean)
    and ResponseJson.Contains('error')
    and (ResponseJson.S['error']='Not authorized') then
  begin
    //调用失败

  end
  else
  begin
    //调用成功
//    uBaseLog.OutputDebugString('调用成功');
    Result:=ParseResponseData;
  end;

//  AStingList.Free;
end;





{ TInterfaceSimpleJsonParam }

constructor TInterfaceSimpleJsonParam.Create;
begin
end;

destructor TInterfaceSimpleJsonParam.Destroy;
begin
  inherited;
end;




{ TInterfaceSimpleJsonParamList }

function TInterfaceSimpleJsonParamList.Add(const Name: String;
                            const Descrip: String): TInterfaceSimpleJsonParam;
begin
  Result:=TInterfaceSimpleJsonParam.Create;
  Inherited Add(Result);
  Result.Name:=Name;
  Result.Descrip:=Descrip;
end;

procedure TInterfaceSimpleJsonParamList.ClearValue;
var
  I:Integer;
begin
  For I:=0 to Self.Count-1 do
  begin
    //varEmpty或varNull
    //Items[I].Value:=varNull;
    FindVarData(Items[I].Value)^.VType := varNull;
  end;
end;

function TInterfaceSimpleJsonParamList.GetItem(Index: Integer): TInterfaceSimpleJsonParam;
begin
  Result:=TInterfaceSimpleJsonParam(Inherited Items[Index]);
end;

function TInterfaceSimpleJsonParamList.GetItemByName(const Name: String): TInterfaceSimpleJsonParam;
var
  I:Integer;
begin
  Result:=nil;
  For I:=0 to Self.Count-1 do
  begin
    if Items[I].Name=Name then
    begin
      Result:=Items[I];
      Break;
    end;
  end;
end;

procedure TInterfaceSimpleJsonParamList.SetValue(const Name: String; const Value: Variant);
begin
  ItemByName[Name].Value:=Value;
end;





{ TInterfaceUrlParamList }

function TInterfaceUrlParamList.Add(const Name: String;
                            const Descrip: String): TInterfaceUrlParam;
begin
  Result:=TInterfaceUrlParam.Create;
  Inherited Add(Result);
  Result.Name:=Name;
  Result.Descrip:=Descrip;
end;

procedure TInterfaceUrlParamList.ClearValue;
var
  I:Integer;
begin
  For I:=0 to Self.Count-1 do
  begin
    //varEmpty或varNull
    //Items[I].Value:=varNull;
    FindVarData(Items[I].Value)^.VType := varNull;
  end;
end;

function TInterfaceUrlParamList.GetItem(Index: Integer): TInterfaceUrlParam;
begin
  Result:=TInterfaceUrlParam(Inherited Items[Index]);
end;

function TInterfaceUrlParamList.GetItemByName(const Name: String): TInterfaceUrlParam;
var
  I:Integer;
begin
  Result:=nil;
  For I:=0 to Self.Count-1 do
  begin
    if Items[I].Name=Name then
    begin
      Result:=Items[I];
      Break;
    end;
  end;
end;

function TInterfaceUrlParamList.GetUrlParamListStr: String;
var
  I:Integer;
  ValueStr:String;
begin
  Result:='';
  for I := 0 to Count-1 do
  begin
    if Not (VarIsNull(Items[I].Value) or (Items[I].Value=unassigned)) then
    begin
      ValueStr:=Items[I].Name+'/'+Items[I].Value;

      if Result<>'' then
      begin
        Result:=Result+'/';
      end;

      Result:=Result+ValueStr;

    end;
  end;
end;


procedure TInterfaceUrlParamList.SetValue(const Name: String; const Value: Variant);
begin
  ItemByName[Name].Value:=Value;
end;


{ THttpControl }

constructor THttpControl.Create;
begin
//  InitializeCriticalSection(FHttpLock);
end;

destructor THttpControl.Destroy;
begin
//  DeleteCriticalSection(FHttpLock);
  inherited;
end;

procedure THttpControl.Lock;
begin
//  EnterCriticalSection(FHttpLock);
end;

procedure THttpControl.UnLock;
begin
//  LeaveCriticalSection(FHttpLock);
end;



{ TApp }

//{$IFDEF VCL}
//
//function TApp.LoadFromXMLNode(AXMLNode: IXMLNode): Boolean;
//begin
//  Result:=False;
//  Self.AppKey:=AXMLNode.Attributes['AppKey'];
//  Self.AppSecret:=AXMLNode.Attributes['AppSecret'];
//  Self.CallBackUrl:=AXMLNode.Attributes['CallBackUrl'];
//  Result:=True;
//end;
//
//function TApp.SaveToXMLNode(AXMLNode: IXMLNode): Boolean;
//begin
//  Result:=False;
//  AXMLNode.Attributes['AppKey']:=Self.AppKey;
//  AXMLNode.Attributes['AppSecret']:=Self.AppSecret;
//  AXMLNode.Attributes['CallBackUrl']:=Self.CallBackUrl;
//  Result:=True;
//end;
//
//function TApp.LoadFromXML(AXMLFilePath: String): Boolean;
//var
//  XMLDoc:TXMLDocument;
//  XMLDocNode:IXMLNode;
//begin
//  Result:=False;
//  if Not FileExists(AXMLFilePath) then Exit;
//  XMLDoc:=TXMLDocument.Create(Application);
//  try
//    XMLDoc.LoadFromFile(AXMLFilePath);
//    XMLDoc.Active := True;
//    XMLDocNode:=XMLDoc.DocumentElement;
//    Result:=LoadFromXMLNode(XMLDocNode);
//  finally
//    XMLDoc.Free;
//  end;
//end;
//
//function TApp.SaveToXML(AXMLFilePath: String): Boolean;
//var
//  XMLDoc:TXMLDocument;
//  XMLDocNode:IXMLNode;
//begin
//  Result:=False;
//  XMLDoc:=TXMLDocument.Create(Application);
//  try
//    XMLDoc.Active := True;
//    XMLDoc.Version := '1.0';
//    XMLDoc.Encoding := 'UTF-8';
//    XMLDocNode:=XMLDoc.AddChild('App');
//    Result:=SaveToXMLNode(XMLDocNode);
//    XMLDoc.SaveToFile(AXMLFilePath);
//  finally
//    XMLDoc.Free;
//  end;
//end;
//{$ENDIF}
//
//
//function TApp.LoadFromINI(AINIFilePath:String):Boolean;
//var
//  AIniFile:TIniFile;
//begin
//  Result:=False;
//
//  AIniFile:=TIniFile.Create(AINIFilePath);
//
//  Self.AppKey:=AIniFile.ReadString('','AppKey','');
//  Self.AppSecret:=AIniFile.ReadString('','AppSecret','');
////  Self.CallBackUrl:=AIniFile.ReadString('','CallBackUrl','');
//
//  AIniFile.Free;
//
//  Result:=True;
//
//end;
//
//function TApp.SaveToINI(AINIFilePath:String):Boolean;
//var
//  AIniFile:TIniFile;
//begin
//
//  Result:=False;
//  AIniFile:=TIniFile.Create(AINIFilePath);
//
//  AIniFile.WriteString('','AppKey',Self.AppKey);
//  AIniFile.WriteString('','AppSecret',Self.AppSecret);
////  AIniFile.WriteString('','CallBackUrl',Self.CallBackUrl);
//
//  AIniFile.Free;
//  Result:=True;
//
//end;
//

{ TInterfaceSetting }

constructor TInterfaceSetting.Create;
begin
  Self.UserId:='apis';
  Self.Password:='35337f1232b9b5c92996e059174ef735';
  Self.Host:='jg.czfood360.cn';
end;

//initialization
//
//finalization
//  if GlobalInterfaceRegistList<>nil then
//  begin
//    GlobalInterfaceRegistList.Clear(True);
//    GlobalInterfaceRegistList.Free;
//  end;


initialization
  GlobalInterfaceSetting:=TInterfaceSetting.Create;

finalization
  FreeAndNil(GlobalInterfaceSetting);

end.






