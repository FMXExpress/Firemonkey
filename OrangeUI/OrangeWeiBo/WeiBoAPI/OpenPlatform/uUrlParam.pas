unit uUrlParam;

interface

uses
//  Windows,
  SysUtils,
  Classes,
  uPublic,
  uFuncCommon,
  uBaseList;

type
  //请求的参数键值对
  TUrlParam=class
  private
    FName:String;
    FValue:Variant;
  public
    constructor Create(const AName:String;const AValue:Variant);
    destructor Destroy;override;
  public
    property Name:String read FName write FName;
    property Value:Variant read FValue write FValue;
  end;

  //参数列表
  TUrlParamList=class(TBaseList)
  private
    function GetItem(Index: Integer): TUrlParam;
  public
    //需要按照名字顺序排序,如果名字相同,那要根据值来排序,生成的签名才是有用的
    function GetUrlParamListStr(const Utf8UrlEncode:Boolean=False):String;
    //添加一对键值
    procedure AddUrlParam(const AName:String;const AValue:Variant);
  public
    procedure Sort;
    function GetItemByName(AName:String):TUrlParam;
    property Items[Index:Integer]:TUrlParam read GetItem;default;
    property ItemByName[AName:String]:TUrlParam read GetItemByName;
  end;

function ParseUrlParamList(Url:String;HasWWW:Boolean):TUrlParamList;
//function GenerateUrl(const AUrl:String;const AParamNames:array of String;const AParamValues:array of Variant):String;

implementation

//function GenerateUrl(const AUrl:String;const AParamNames:array of String;const AParamValues:array of Variant):String;
//var
//  I: Integer;
//  AParamList:TUrlParamList;
//begin
//  Result:='';
//  AParamList:=TUrlParamList.Create;
//  Try
//    for I:=0 to Length(AParamNames) - 1 do
//    begin
//      AParamList.AddUrlParam(AParamNames[I],AParamValues[I]);
//    end;
//    AParamList.Sort;
//    Result:=AUrl+'?'+AParamList.GetUtf8UrlEncodeUrlParamListStr;
//  Finally
//    AParamList.Free;
//  End;
//end;

function ParseUrlParamList(Url:String;HasWWW:Boolean):TUrlParamList;
var
  I: Integer;
  AName,AValue:String;
  AEqualCharIndex:Integer;
  AParamStrList:TStringList;
  AParamListString:String;
  AParamListStringStartIndex:Integer;
begin
  Result:=TUrlParamList.Create;
  if HasWWW then
  begin
    //解析出参数列表字符串
    AParamListStringStartIndex:=Pos('?',Url);
    AParamListString:=Copy(Url,AParamListStringStartIndex+1,MaxInt);
  end
  else
  begin
    AParamListString:=Url;
  end;

  if AParamListString<>'' then
  begin
    //找到参数列表字符串
    AParamStrList:=TStringList.Create;
    Try
      AParamStrList.Delimiter:='&';
      AParamStrList.DelimitedText:=AParamListString;
      for I := 0 to AParamStrList.Count-1 do
      begin
        if (AParamStrList[I]<>'') then
        begin
          AEqualCharIndex:=Pos('=',AParamStrList[I]);
          if AEqualCharIndex>0 then
          begin
            AName:=Copy(AParamStrList[I],1,AEqualCharIndex-1);
            AValue:=Copy(AParamStrList[I],AEqualCharIndex+1,Length(AParamStrList[I])-AEqualCharIndex);
            Result.AddUrlParam(AName,AValue);
          end;
        end;
      end;
    Finally
      FreeAndNil(AParamStrList);
    End;
  end;
end;

{ TUrlParam }

constructor TUrlParam.Create(const AName:String;const AValue:Variant);
begin
  FName:=AName;
  FValue:=AValue;
end;

destructor TUrlParam.Destroy;
begin
  inherited;
end;

{ TUrlParamList }


procedure TUrlParamList.AddUrlParam(const AName: String;const AValue: Variant);
begin
  Self.Add(TUrlParam.Create(AName,AValue));
end;

function TUrlParamList.GetItemByName(AName: String): TUrlParam;
var
  I:Integer;
begin
  Result:=nil;
  for I := 0 to Self.Count - 1 do
  begin
    if Items[I].Name=AName then
    begin
      Result:=Items[I];
      Break;
    end;
  end;
end;

function TUrlParamList.GetItem(Index: Integer): TUrlParam;
begin
  Result:=TUrlParam(Inherited Items[Index]);
end;

function TUrlParamList.GetUrlParamListStr(const Utf8UrlEncode:Boolean=False): String;
var
  I:Integer;
  ValueStr:String;
begin
  Result:='';
  for I := 0 to Count-1 do
  begin
    ValueStr:=Items[I].Value;
    if ValueStr<>'' then
    begin
      if Result<>'' then
      begin
        Result:=Result+'&';
//        UTF8Encode
      end;
//      if Utf8UrlEncode then
//      begin
//        Result:=Result+Items[I].Name+'='+UrlEncodeUTF8(UTF8Encode(ValueStr));
//      end
//      else
//      begin
        Result:=Result+Items[I].Name+'='+ValueStr;
//      end;
    end;
  end;
end;

function SortByName_Compare(Item1, Item2: Pointer): Integer;
var
  Param1,Param2:TUrlParam;
begin
  Param1:=TUrlParam(Item1);
  Param2:=TUrlParam(Item2);
  if Param1.FName>Param2.FName then
  begin
    Result:=1;
  end
  else if Param1.FName<Param2.FName then
  begin
    Result:=-1;
  end
  else
  begin
    Result:=0;
  end;
end;

procedure TUrlParamList.Sort;
begin
  FItems.Sort(SortByName_Compare);
end;

end.

