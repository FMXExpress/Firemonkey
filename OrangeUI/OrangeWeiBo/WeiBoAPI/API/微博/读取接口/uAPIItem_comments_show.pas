//http://open.weibo.com/wiki/2/comments/show
unit uAPIItem_comments_show;

interface

uses
  Classes,
  SysUtils,
  XSuperObject,
  uFuncCommon,
  uDataStructure,
  uOpenPlatform;

type
  TAPIResponse_comments_show=class(TAPIResponse)
  protected
    function ParseDataStructure:Boolean;override;
  public
    comments:Tcomments;
//    "previous_cursor":0,
//    "next_cursor":3669625160565342,
//    "total_number":2020
    previous_cursor:int;//":0,
    next_cursor:int;//":3669625160565342,
    total_number:int;//":2020
    constructor Create;
    destructor Destroy;override;
  end;

  TAPIItem_comments_show=class(TAPIItem)
  protected
    procedure Init;override;
  end;

implementation

{ TAPIItem_comments_show }

procedure TAPIItem_comments_show.Init;
begin
  inherited;
  Name:='comments/show';
  Descrip:='根据微博ID返回某条微博的评论列表';
  Url:='https://api.weibo.com/2/comments/show.json';
  HttpRequestMethods:=[hrmGet];
  ResponseDataType:=rdtData;
  ResponseDataFormat:=rdfJson;
  NeedLogin:=True;

  Self.ParamList.Add('source',false,'string','采用OAuth授权方式不需要此参数，其他授权方式为必填参数，数值为应用的AppKey。  ');
  Self.ParamList.Add('access_token',false,'string','采用OAuth授权方式为必填参数，其他授权方式不需要此参数，OAuth授权后获得。  ');
  Self.ParamList.Add('id',true,'int64','需要查询的微博ID。  ');
  Self.ParamList.Add('since_id',false,'int64','若指定此参数，则返回ID比since_id大的评论（即比since_id时间晚的评论），默认为0。  ');
  Self.ParamList.Add('max_id',false,'int64','若指定此参数，则返回ID小于或等于max_id的评论，默认为0。  ');
  Self.ParamList.Add('count',false,'int','单页返回的记录条数，默认为50。  ');
  Self.ParamList.Add('page',false,'int','返回结果的页码，默认为1。  ');
  Self.ParamList.Add('filter_by_author',false,'int','作者筛选类型，0：全部、1：我关注的人、2：陌生人，默认为0。  ');


end;

{ TAPIResponse_comments_show }

constructor TAPIResponse_comments_show.Create;
begin
  comments:=Tcomments.Create;
end;

destructor TAPIResponse_comments_show.Destroy;
begin
  FreeAndNil(comments);
  inherited;
end;

function TAPIResponse_comments_show.ParseDataStructure: Boolean;
var
  I: Integer;
  Acomment:Tcomment;
  AcommentJson:ISuperObject;
  AcommentsJson:ISuperObject;
begin
  Result:=False;

  comments.Clear(True);

  if RootJson<>nil then
  begin
    AcommentsJson:=RootJson.O['comments'];
    if AcommentsJson<>nil then
    begin
//      for I := 0 to AcommentsJson.AsArray.Length - 1 do
//      begin
//        AcommentJson:=AcommentsJson.AsArray[I];
//        if AcommentJson<>nil then
//        begin
//          Acomment:=Tcomment.Create;
//          Acomment.ParseFromJson(AcommentJson);
//          comments.Add(Acomment);
//        end;
//      end;
    end;

    previous_cursor:=RootJson.I['previous_cursor'];//":0,
    next_cursor:=RootJson.I['next_cursor'];//":3669625160565342,
    total_number:=RootJson.I['total_number'];//":2020
  end;

  Result:=True;
end;


initialization
  RegisterAPIItem('comments/show',TAPIItem_comments_show);

end.
