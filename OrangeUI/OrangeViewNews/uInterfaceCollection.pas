unit uInterfaceCollection;

interface


uses
  SysUtils,
  uFuncCommon,
  Classes,
  uBaseLog,
  XSuperJson,
  XSuperObject,
  uInterfaceData,
  uInterfaceClass;

type


  {$Region '新闻接口'}
  //获取新闻未读数量
  TInterface_GetUnReadNewsCount=class(TInterfaceItem)
  public
    status:Boolean;
    result_:Integer;
  protected
    function ParseResponseData:Boolean;override;
  public
    constructor Create;override;
    destructor Destroy;override;
  end;



//  //获取新闻的类别字典
//  TInterface_GetNewsCategory=class(TInterfaceItem)
//  public
//    status:Boolean;
//    result_:TNewsCategoryList;
//  protected
//    function ParseResponseData:Boolean;override;
//  public
//    constructor Create;override;
//    destructor Destroy;override;
//  end;



  //获取新闻列表
  TInterface_GetNewsList=class(TInterfaceItem)
  public
    status:Boolean;
    total:Integer;
    result_:TNewsList;
  protected
    function ParseResponseData:Boolean;override;
  public
    constructor Create;override;
    destructor Destroy;override;
  end;




  //新闻详细页
  TInterface_GetNewsDetail=class(TInterfaceItem)
  public
  public
    constructor Create;override;
    destructor Destroy;override;
  end;
  {$EndRegion}


  {$Region '字典接口'}
  //获取字典列表
  TInterface_GetDictList=class(TInterfaceItem)
  public
    status:Boolean;
    error:String;
    result_:TDictList;
  protected
    function ParseResponseData:Boolean;override;
  public
    constructor Create;override;
    destructor Destroy;override;
  end;
  {$EndRegion}





implementation




{ TInterface_GetDictList }

constructor TInterface_GetDictList.Create;
begin
  inherited;
//  Self.Name:='GetDictList';
  Self.URL:='/rest/dict/index/';
  Self.Descrip:='获取字典列表';
  Self.HttpRequestMethods:=[hrmGet];

end;

destructor TInterface_GetDictList.Destroy;
begin
//  FreeAndNil(modified);
  FreeAndNil(result_);
  inherited;
end;

function TInterface_GetDictList.ParseResponseData: Boolean;
var
  I: Integer;
  ADict:TDict;
//  ADickJson:ISuperObject;
  ADictListJson:ISuperArray;
begin
  Result:=False;
//  FreeAndNil(modified);
  FreeAndNil(result_);

  if (ResponseJson<>nil) and ResponseJson.Contains('status') then
  begin

    status:=ResponseJson.B['status'];
    error:=ResponseJson.S['error'];

//    modified:=TModifiedTime.Create;
//    modified.ParseFromJson(ResponseJson.o['modified']);


    if status then
    begin
      result_:=TDictList.Create;
      ADictListJson:=ResponseJson.A['result'];
      for I := 0 to ADictListJson.Length - 1 do
      begin
//        ADictJson:=ADictListJson.O[I];

        ADict:=TDict.Create;
        ADict.ParseFromJson(ADictListJson.O[I]);
        result_.Add(ADict);

      end;
    end;

    Result:=True;
  end;
end;


{ TInterface_GetNewsDetail }

constructor TInterface_GetNewsDetail.Create;
begin
  inherited;
//  Self.Name:='GetNewsDetail';
  Self.URL:='/mobile/news/detail/';
  Self.Descrip:='新闻详细页';
  Self.HttpRequestMethods:=[hrmGet];

  ResponseDataType:=rdtHtml;

  Self.UrlParamList.Add('id','新闻ID');
//URL组织格式：http://jg.czfood360.cn/mobile/news/detail/id/新闻的ID
//返回类型为：HTML
end;

destructor TInterface_GetNewsDetail.Destroy;
begin
  inherited;
end;





{ TInterface_GetNewsList }

constructor TInterface_GetNewsList.Create;
begin
  inherited;
//  Self.Name:='GetNewsList';
  Self.URL:='/rest/news/mobile/';
  Self.Descrip:='获取新闻列表';
  Self.HttpRequestMethods:=[hrmGet];

//  Self.UrlParamList.Add('region_code','区域码');

  Self.UrlParamList.Add('offset','结果集偏移量');
  Self.UrlParamList.Add('limit','每页的纪录数');
  Self.UrlParamList.Add('category','分类');

end;

destructor TInterface_GetNewsList.Destroy;
begin
  FreeAndNil(result_);
  inherited;
end;

function TInterface_GetNewsList.ParseResponseData: Boolean;
var
  I: Integer;
  ANews:TNews;
//  ANewsJson:ISuperObject;
  ANewsListJson:ISuperArray;
begin
  Result:=False;
  FreeAndNil(result_);

  if (ResponseJson<>nil) and ResponseJson.Contains('status') then
  begin

    status:=ResponseJson.B['status'];
    total:=ResponseJson.I['total'];

    if status then
    begin
      result_:=TNewsList.Create;
      ANewsListJson:=ResponseJson.A['result'];
      for I := 0 to ANewsListJson.Length - 1 do
      begin
//        ANewsJson:=ANewsListJson.O[I];

        ANews:=TNews.Create;
        ANews.ParseFromJson(ANewsListJson.O[I]);
        result_.Add(ANews);

      end;
    end;

    Result:=True;
  end;
end;


//{ TInterface_GetNewsCategory }
//
//constructor TInterface_GetNewsCategory.Create;
//begin
//  inherited;
////  Self.Name:='GetNewsCategory';
//  Self.URL:='/rest/dict/items/module/news/code/category/';
//  Self.Descrip:='获取新闻的类别字典';
//  Self.HttpRequestMethods:=[hrmGet];
//
//end;
//
//destructor TInterface_GetNewsCategory.Destroy;
//begin
////  FreeAndNil(modified);
//  FreeAndNil(result_);
//  inherited;
//end;
//
//function TInterface_GetNewsCategory.ParseResponseData: Boolean;
//var
//  I: Integer;
//  ANewsCategory:TNewsCategory;
////  ANewsCategoryJson:ISuperObject;
//  ANewsCategoriesJson:ISuperArray;
//begin
//  Result:=False;
////  FreeAndNil(modified);
//  FreeAndNil(result_);
//
//  if (ResponseJson<>nil) and ResponseJson.Contains('status') then
//  begin
//
//    status:=ResponseJson.B['status'];
////    modified:=TModifiedTime.Create;
////    modified.ParseFromJson(ResponseJson.o['modified']);
//
//    if status then
//    begin
//      result_:=TNewsCategoryList.Create;
//      ANewsCategoriesJson:=ResponseJson.A['result'];
//      for I := 0 to ANewsCategoriesJson.Length - 1 do
//      begin
////        ANewsCategoryJson:=ANewsCategoriesJson.O[I];
//
//        ANewsCategory:=TNewsCategory.Create;
//        ANewsCategory.ParseFromJson(ANewsCategoriesJson.O[I]);
//        result_.Add(ANewsCategory);
//
//      end;
//    end;
//
//    Result:=True;
//  end;
//
//end;



{ TInterface_GetUnReadNewsCount }

constructor TInterface_GetUnReadNewsCount.Create;
begin
  inherited;
//  Self.Name:='GetUnReadNewsCount';
  Self.URL:='/rest/news/unread/';
  Self.Descrip:='获取新闻未读数量';
  Self.HttpRequestMethods:=[hrmGet];



end;

destructor TInterface_GetUnReadNewsCount.Destroy;
begin
  inherited;
end;

function TInterface_GetUnReadNewsCount.ParseResponseData: Boolean;
begin

  Result:=False;

  if (ResponseJson<>nil) and ResponseJson.Contains('status') then
  begin

    status:=ResponseJson.B['status'];
    result_:=ResponseJson.I['result'];

    Result:=True;
  end;

end;



end.



