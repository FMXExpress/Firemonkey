unit uInterfaceData;

interface

uses
  IniFiles,
  SysUtils,
  uFuncCommon,
  Classes,
  StrUtils,
  Variants,
  DB,
  XSuperJson,
  XSuperObject,
  uDirectoryPublic,
  uBaseList,
  uFileCommon;




type



  {$Region '字典'}
  TDict=class;
  TDictValue=class
  public
    name:String;//"监管员",
    value:String;//3,
    sortorder:Int64;//1
  public
    Dict:TDict;
    constructor Create(ADict:TDict);
    function ParseFromJson(AJson: ISuperObject): Boolean;
  end;
  TDictValueList=class(TBaseList)
  private
    function GetItem(Index: Integer): TDictValue;
  public
    Dict:TDict;
    constructor Create(ADict:TDict;AObjectOwnership:TObjectOwnership);
    function ParseFromJsonArray(JsonArray:ISuperArray):Boolean;
    function FindByName(name:String):TDictValue;
    function FindByValue(value:String):TDictValue;
    property Items[Index:Integer]:TDictValue read GetItem;default;
  end;
  TDict=class
  public
    module:String;//"team",
    code:String;//"rank",
    name:String;//"监管类别",
    items:TDictValueList;//[
  public
    function ParseFromJson(AJson: ISuperObject): Boolean;
    constructor Create;
    destructor Destroy;override;
  end;
  TDictList=class(TBaseList)
  private
    function GetItem(Index: Integer): TDict;
  public
    function FindItemByModuleCode(module:String;code:String):TDict;
    property Items[Index:Integer]:TDict read GetItem;default;
  end;
  {$EndRegion}




  {$Region '新闻'}
//  //新闻分类
//  TNewsCategory=class
//  public
//    name:String;//"最新动态",
//    value:Int64;//1,
//    sortorder:Int64;//50
//  public
//    function ParseFromJson(AJson: ISuperObject): Boolean;
//  end;
//  TNewsCategoryList=class(TBaseList)
//  private
//    function GetItem(Index: Integer): TNewsCategory;
//  public
//    property Items[Index:Integer]:TNewsCategory read GetItem;default;
//  end;
//
  //新闻
  TNews=class
  public
    id:String;//"a08b3043-025e-11e4-9154-00163e020f4a",
    title:String;//"统筹配合 团结协作 共同打好防汛攻坚战               ——戚街道召开2014防汛防灾工作会议 ",
    date:String;//"2014-07-03",
    is_read:Int64;//0,
    image:String;//"/uploads/20140703/14043565111086.jpg",
    intro:String;//"为了扎实做好今年防汛工作，7月2日下午，戚街道办事处召开了2014年"
  public
    IsDownloadingImage:Boolean;
    IsDownloadedImage:Boolean;
    function URL:String;
    function LocalImagePath:String;
    function RemoteImagePath:String;
    function ParseFromJson(AJson: ISuperObject): Boolean;
  end;
  TNewsList=class(TBaseList)
  private
    function GetItem(Index: Integer): TNews;
  public
    property Items[Index:Integer]:TNews read GetItem;default;
  end;
  {$EndRegion}



function CreateStringListFromJsonArray(JsonArray:ISuperArray):TStringList;
function GetJsonDoubleValue(AJson: ISuperObject;Name:String):Double;
function GetJsonStringValue(AJson: ISuperObject;Name:String):String;


function CreateGUIDString:String;

implementation

uses
  uInterfaceClass;


function CreateGUIDString:String;
var
  guid:TGUID;
begin
  CreateGUID(guid);
  Result:=GUIDToString(guid);
  Result:=ReplaceStr(Result,'{','');
  Result:=ReplaceStr(Result,'}','');
  Result:=ReplaceStr(Result,'-','');
end;




function GetJsonDoubleValue(AJson: ISuperObject;Name:String):Double;
begin
//  if AJson.Null[Name]=jNull then
//  begin
//    Result:='';
//  end
//  else
//  begin
//    Result:=AJson.V[Name];
//  end;
  if VarIsNull(AJson.V[Name]) then
  begin
    Result:=0.00;
  end
  else
  begin
    Result:=AJson.V[Name];
  end;
end;

function GetJsonStringValue(AJson: ISuperObject;Name:String):String;
begin
//  if AJson.Null[Name]=jNull then
//  begin
//    Result:='';
//  end
//  else
//  begin
//    Result:=AJson.V[Name];
//  end;
  if VarIsNull(AJson.V[Name]) then
  begin
    Result:='';
  end
  else
  begin
    Result:=AJson.V[Name];
  end;

end;

function CreateStringListFromJsonArray(JsonArray:ISuperArray):TStringList;
var
  I:Integer;
begin
  Result:=TStringList.Create;
  for I := 0 to JsonArray.Length - 1 do
  begin
    Result.Add(JsonArray.S[I]);
  end;
end;

function LoadStringListFromJsonStrArray(StringList:TStringList;JsonArray:ISuperArray):Boolean;
var
  I:Integer;
begin
  Result:=False;
  StringList.Clear;
  for I := 0 to JsonArray.Length - 1 do
  begin
    StringList.Add(JsonArray.S[I]);
  end;
  Result:=True;
end;


function LoadStringListFromJsonIntArray(StringList:TStringList;JsonArray:ISuperArray):Boolean;
var
  I:Integer;
begin
  Result:=False;
  StringList.Clear;
  for I := 0 to JsonArray.Length - 1 do
  begin
    StringList.Add(IntToStr(JsonArray.I[I]));
  end;
  Result:=True;
end;




{ TNews }

function TNews.LocalImagePath: String;
begin
  //http://jg.czfood360.cn/editor/ueditor/dialogs/attachment/fileTypeImages/icon_rar.gif
  if (Pos('://',image)<=0) and (Pos('/',image)<=0) then
  begin
    Result:=GetAppDataDir_NewsImage(False)+ReplaceStr(image,'/',PathDelim)+'.png';
  end
  else
  begin
    Result:=GetAppDataDir_NewsImage+ReplaceStr(id,'-','_')+'.png';
  end;
end;

function TNews.RemoteImagePath: String;
begin
//  if (Pos('://',image)<=0) and (Pos('/',image)<=0) then
  if (Pos('://',image)<=0) then
  begin
    Result:='http://'+GlobalInterfaceSetting.Host+image;
  end
  else
  begin
    Result:=image;
  end;
end;

function TNews.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  Result:=False;

  id:=AJson.S['id'];//"a08b3043-025e-11e4-9154-00163e020f4a",
  title:=AJson.S['title'];//"统筹配合 团结协作 共同打好防汛攻坚战               ——戚街道召开2014防汛防灾工作会议 ",
  date:=AJson.S['date'];//"2014-07-03",
  is_read:=AJson.I['is_read'];//0,
  image:=AJson.S['image'];//"/uploads/20140703/14043565111086.jpg",
  intro:=AJson.S['intro'];//"为了扎实做好今年防汛工作，7月2日下午，戚街道办事处召开了2014年"

  Result:=True;
end;

function TNews.URL: String;
begin
  Result:='http://'+GlobalInterfaceSetting.Host+'/mobile/news/detail/id/'+id;
end;

{ TNewsList }

function TNewsList.GetItem(Index: Integer): TNews;
begin
  Result:=TNews(Inherited Items[Index]);
end;




{ TDictList }

function TDictList.FindItemByModuleCode(module, code: String): TDict;
var
  I: Integer;
begin
  Result:=nil;
  for I := 0 to Self.Count-1 do
  begin
    if (Self.Items[I].module=module) and (Self.Items[I].code=code) then
    begin
      Result:=Self.Items[I];
      Break;
    end;
  end;
end;

function TDictList.GetItem(Index: Integer): TDict;
begin
  Result:=TDict(Inherited Items[Index]);
end;

{ TDict }

constructor TDict.Create;
begin
  items:=TDictValueList.Create(Self,ooOwned);
end;

destructor TDict.Destroy;
begin
  FreeAndNil(items);
  inherited;
end;

function TDict.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  Result:=False;

  module:=AJson.S['module'];//"team",
  code:=GetJsonStringValue(AJson,'code');//"rank",
  name:=AJson.S['name'];//"监管类别",
  items.ParseFromJsonArray(AJson.A['items']);//[

  Result:=True;
end;

{ TDictValue }


constructor TDictValue.Create(ADict: TDict);
begin
  Dict:=ADict;
end;

function TDictValue.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  Result:=False;

  name:=AJson.S['name'];//"监管员",
  value:=GetJsonStringValue(AJson,'value');//3,
  sortorder:=AJson.I['sortorder'];//1

  Result:=True;
end;

{ TDictValueList }

constructor TDictValueList.Create(ADict: TDict;AObjectOwnership:TObjectOwnership);
begin
  Inherited Create(AObjectOwnership);
  Dict:=ADict;
end;

function TDictValueList.FindByName(name: String): TDictValue;
var
  I: Integer;
begin
  Result:=nil;
  for I := 0 to Self.Count-1 do
  begin
    if Self.Items[I].name=name then
    begin
      Result:=Self.Items[I];
      Break;
    end;
  end;
end;

function TDictValueList.FindByValue(value: String): TDictValue;
var
  I: Integer;
begin
  Result:=nil;
  for I := 0 to Self.Count-1 do
  begin
    if Self.Items[I].value=value then
    begin
      Result:=Self.Items[I];
      Break;
    end;
  end;
end;

function TDictValueList.GetItem(Index: Integer): TDictValue;
begin
  Result:=TDictValue(Inherited Items[Index]);
end;

function TDictValueList.ParseFromJsonArray(JsonArray: ISuperArray): Boolean;
var
  I:Integer;
  ADictValue:TDictValue;
begin
  Result:=False;

  for I := 0 to JsonArray.Length - 1 do
  begin
    ADictValue:=TDictValue.Create(Dict);
    ADictValue.ParseFromJson(JsonArray.O[I]);
    Self.Add(ADictValue);
  end;

  Result:=True;
end;



end.
