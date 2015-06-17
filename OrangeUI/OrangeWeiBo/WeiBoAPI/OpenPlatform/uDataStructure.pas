unit uDataStructure;

interface

uses
//  Windows,
  IniFiles,
  SysUtils,
  XSuperObject,
  uFuncCommon,
  uBaseList;


type
  int=Int64;
  _object=TObject;
  _object_array=array of _object;
 Tuser=class;
 Tgeo=class;




  Tstatus=class
  public
    destructor Destroy;override;
    function ParseFromJson(AJson: ISuperObject): Boolean;
  public
    created_at	:string	;//微博创建时间
    id	:int64	;//微博ID
    mid	:int64	;//微博MID
    idstr	:string	;//字符串型的微博ID
    text	:string	;//微博信息内容
    source	:string	;//微博来源
    favorited	:boolean	;//是否已收藏，true：是，false：否
    truncated	:boolean	;//是否被截断，true：是，false：否
    in_reply_to_status_id	:string	;//（暂未支持）回复ID
    in_reply_to_user_id	:string	;//（暂未支持）回复人UID
    in_reply_to_screen_name	:string	;//（暂未支持）回复人昵称
    thumbnail_pic	:string	;//缩略图片地址，没有时不返回此字段
    bmiddle_pic	:string	;//中等尺寸图片地址，没有时不返回此字段
    original_pic	:string	;//原始图片地址，没有时不返回此字段
    geo	:Tgeo	;//地理信息字段 详细
    user	:Tuser	;//微博作者的用户信息字段 详细
    retweeted_status	:Tstatus	;//被转发的原微博信息字段，当该微博为转发微博时返回 详细
    reposts_count	:int	;//转发数
    comments_count	:int	;//评论数
    attitudes_count	:int	;//表态数
    mlevel	:int	;//暂未支持
    visible	:_object	;//微博的可见性及指定可见分组信息。该object中type取值，0：普通微博，1：私密微博，3：指定分组微博，4：密友微博；list_id为分组的组号
    pic_urls	:_object	;//微博配图地址。多图时返回多图链接。无配图返回“[]”
    ad	:_object_array;//object array	;//微博流内的推广微博ID




  end;

  Tstatuses=class(TBaseList)
  private
    function GetItem(Index: Integer): Tstatus;
  public
//    function FindItemBytid(tid:Int64):TTrade;
//    function FindItemIndexBytid(tid:Int64):Integer;
    property Items[Index:Integer]:Tstatus read GetItem;default;
  end;


  Tcomment=class
  public
    destructor Destroy;override;
    function ParseFromJson(AJson: ISuperObject): Boolean;
  public

    created_at  :string;//  评论创建时间
    id  :int64;//  评论的ID
    text  :string;//  评论的内容
    source  :string;//  评论的来源
    user  :Tuser;//  评论作者的用户信息字段 详细
    mid  :string;//  评论的MID
    idstr  :string;//  字符串型的评论ID
    status  :Tstatus;//  评论的微博信息字段 详细
    reply_comment  :_object;//  评论来源评论，当本评论属于对另一评论的回复时返回此字段

  end;



  Tcomments=class(TBaseList)
  private
    function GetItem(Index: Integer): Tcomment;
  public
//    function FindItemBytid(tid:Int64):TTrade;
//    function FindItemIndexBytid(tid:Int64):Integer;
    property Items[Index:Integer]:Tcomment read GetItem;default;
  end;




  Tuser=class
  public
    function LoadFromINI(AINIFilePath:String):Boolean;
    function SaveToINI(AINIFilePath:String):Boolean;
    function ParseFromJson(AJson: ISuperObject): Boolean;
  public
    id  :int64  ;//用户UID
    idstr  :string  ;//字符串型的用户UID
    screen_name  :string  ;//用户昵称
    name  :string  ;//友好显示名称
    province  :int  ;//用户所在省级ID
    city  :int  ;//用户所在城市ID
    location  :string  ;//用户所在地
    description  :string  ;//用户个人描述
    url  :string  ;//用户博客地址
    profile_image_url  :string  ;//用户头像地址（中图），50×50像素
    profile_url  :string  ;//用户的微博统一URL地址
    domain  :string  ;//用户的个性化域名
    weihao  :string  ;//用户的微号
    gender  :string  ;//性别，m：男、f：女、n：未知
    followers_count  :int  ;//粉丝数
    friends_count  :int  ;//关注数
    statuses_count  :int  ;//微博数
    favourites_count  :int  ;//收藏数
    created_at  :string  ;//用户创建（注册）时间
    following  :boolean  ;//暂未支持
    allow_all_act_msg  :boolean  ;//是否允许所有人给我发私信，true：是，false：否
    geo_enabled  :boolean  ;//是否允许标识用户的地理位置，true：是，false：否
    verified  :boolean  ;//是否是微博认证用户，即加V用户，true：是，false：否
    verified_type  :int  ;//暂未支持
    remark  :string  ;//用户备注信息，只有在查询用户关系时才返回此字段
    status  :_object  ;//用户的最近一条微博信息字段 详细
    allow_all_comment  :boolean  ;//是否允许所有人对我的微博进行评论，true：是，false：否
    avatar_large  :string  ;//用户头像地址（大图），180×180像素
    avatar_hd  :string  ;//用户头像地址（高清），高清头像原图
    verified_reason  :string  ;//认证原因
    follow_me  :boolean  ;//该用户是否关注当前登录用户，true：是，false：否
    online_status  :int  ;//用户的在线状态，0：不在线、1：在线
    bi_followers_count  :int  ;//用户的互粉数
    lang  :string  ;//用户当前的语言版本，zh-cn：简体中文，zh-tw：繁体中文，en：英语
  end;

  Tuser_count=class
  public
    function ParseFromJson(AJson: ISuperObject): Boolean;
  public
    //id  int64  微博ID
    //followers_count  int  粉丝数
    //friends_count  int  关注数
    //statuses_count  int  微博数
    //private_friends_count  int  暂未支持
    id  :int64  ;//用户UID
    followers_count  :int  ;//粉丝数
    friends_count  :int  ;//关注数
    statuses_count  :int  ;//微博数
//    private_friends_count  :int  ;//收藏数
  end;

  Tuser_counts=class(TBaseList)
  private
    function GetItem(Index: Integer): Tuser_count;
  public
    property Items[Index:Integer]:Tuser_count read GetItem;default;
  end;

  Tgeo=class
  public
    function ParseFromJson(AJson: ISuperObject): Boolean;
  public
//    地理信息（geo）
//    返回值字段  字段类型  字段说明
    longitude  :string  ;//经度坐标
    latitude  :string  ;//维度坐标
    city  :string  ;//所在城市的城市代码
    province  :string  ;//所在省份的省份代码
    city_name  :string  ;//所在城市的城市名称
    province_name  :string  ;//所在省份的省份名称
    address  :string  ;//所在的实际地址，可以为空
    pinyin  :string  ;//地址的汉语拼音，不是所有情况都会返回该字段
    more  :string  ;//更多信息，不是所有情况都会返回该字段
  end;



implementation

//FLocalDateTime:=Rfc822ToDateTime(FDate,ParseDateSucc,FTimeZone,FUTCDateTime);

{ Tstatus }

destructor Tstatus.Destroy;
begin
  FreeAndNil(user);
  FreeAndNil(retweeted_status);
  inherited;
end;

function Tstatus.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  Result:=False;

  //created_at	string	微博创建时间
  created_at:=AJson.S['created_at'];
  //id	int64	微博ID
  id:=AJson.I['id'];
  //mid	int64	微博MID
//  mid:=AJson.I['mid'];
  //idstr	string	字符串型的微博ID
  idstr:=AJson.S['idstr'];
  //text	string	微博信息内容
  text:=AJson.S['text'];
  //source	string	微博来源
  source:=AJson.S['source'];
  //favorited	boolean	是否已收藏，true：是，false：否
  //truncated	boolean	是否被截断，true：是，false：否
  //in_reply_to_status_id	string	（暂未支持）回复ID
  in_reply_to_status_id:=AJson.S['in_reply_to_status_id'];
  //in_reply_to_user_id	string	（暂未支持）回复人UID
  in_reply_to_user_id:=AJson.S['in_reply_to_user_id'];
  //in_reply_to_screen_name	string	（暂未支持）回复人昵称
  in_reply_to_screen_name:=AJson.S['in_reply_to_screen_name'];
  //thumbnail_pic	string	缩略图片地址，没有时不返回此字段
  thumbnail_pic:=AJson.S['thumbnail_pic'];
  //bmiddle_pic	string	中等尺寸图片地址，没有时不返回此字段
  bmiddle_pic:=AJson.S['bmiddle_pic'];
  //original_pic	string	原始图片地址，没有时不返回此字段
  original_pic:=AJson.S['original_pic'];
  //geo	object	地理信息字段 详细
//  if AJson.Contains('geo') then
//  begin
//    geo:=Tgeo.Create;
//    Tgeo(geo).ParseFromJson(AJson.O['geo']);
//  end;
  //user	object	微博作者的用户信息字段 详细
  if AJson.Contains('user') then
  begin
    user:=Tuser.Create;
    Tuser(user).ParseFromJson(AJson.O['user']);
  end;
  //retweeted_status	object	被转发的原微博信息字段，当该微博为转发微博时返回 详细
  if AJson.Contains('retweeted_status') then
  begin
    retweeted_status:=Tstatus.Create;
    Tstatus(retweeted_status).ParseFromJson(AJson.O['retweeted_status']);
  end;
  //reposts_count	int	转发数
  reposts_count:=AJson.I['reposts_count'];
  //comments_count	int	评论数
  comments_count:=AJson.I['comments_count'];
  //attitudes_count	int	表态数
  attitudes_count:=AJson.I['attitudes_count'];
  //mlevel	int	暂未支持
  mlevel:=AJson.I['mlevel'];
  //visible	object	微博的可见性及指定可见分组信息。该object中type取值，0：普通微博，1：私密微博，3：指定分组微博，4：密友微博；list_id为分组的组号
  //pic_urls	object	微博配图地址。多图时返回多图链接。无配图返回“[]”
  //ad	object array	微博流内的推广微博ID



  Result:=True;
end;

{ Tstatuses }

function Tstatuses.GetItem(Index: Integer): Tstatus;
begin
  Result:=Tstatus(Inherited Items[Index]);
end;

{ Tuser }

function Tuser.LoadFromINI(AINIFilePath: String): Boolean;
var
  AIniFile:TIniFile;
begin
  Result:=False;

  AIniFile:=TIniFile.Create(AINIFilePath);

  //id  int64  用户UID
  id:=AIniFile.ReadInteger('','id',0);
  //idstr  string  字符串型的用户UID
  idstr:=AIniFile.ReadString('','idstr','');
  //screen_name  string  用户昵称
  screen_name:=AIniFile.ReadString('','screen_name','');
  //name  string  友好显示名称
  name:=AIniFile.ReadString('','name','');
////  //province  int  用户所在省级ID
////  province:=AJson.I['province'];
////  //city  int  用户所在城市ID
////  city:=AJson.I['city'];
//  //location  string  用户所在地
//  location:=AJson.S['location'];
//  //description  string  用户个人描述
//  description:=AJson.S['description'];
//  //url  string  用户博客地址
//  url:=AJson.S['url'];
//  //profile_image_url  string  用户头像地址（中图），50×50像素
//  profile_image_url:=AJson.S['profile_image_url'];
//  //profile_url  string  用户的微博统一URL地址
//  profile_url:=AJson.S['profile_url'];
//  //domain  string  用户的个性化域名
//  domain:=AJson.S['domain'];
//  //weihao  string  用户的微号
//  weihao:=AJson.S['weihao'];
//  //gender  string  性别，m：男、f：女、n：未知
//  gender:=AJson.S['gender'];
  //followers_count  int  粉丝数
  followers_count:=AIniFile.ReadInteger('','followers_count',0);
  //friends_count  int  关注数
  friends_count:=AIniFile.ReadInteger('','friends_count',0);
  //statuses_count  int  微博数
  statuses_count:=AIniFile.ReadInteger('','statuses_count',0);
  //favourites_count  int  收藏数
  favourites_count:=AIniFile.ReadInteger('','favourites_count',0);
//  //created_at  string  用户创建（注册）时间
//  created_at:=AJson.S['created_at'];
//  //following  boolean  暂未支持
//  following:=AJson.B['following'];
//  //allow_all_act_msg  boolean  是否允许所有人给我发私信，true：是，false：否
//  allow_all_act_msg:=AJson.B['allow_all_act_msg'];
//  //geo_enabled  boolean  是否允许标识用户的地理位置，true：是，false：否
//  geo_enabled:=AJson.B['geo_enabled'];
//  //verified  boolean  是否是微博认证用户，即加V用户，true：是，false：否
//  verified:=AJson.B['verified'];
//  //verified_type  int  暂未支持
//  verified_type:=AJson.I['verified_type'];
//  //remark  string  用户备注信息，只有在查询用户关系时才返回此字段
//  remark:=AJson.S['remark'];
////  //status  object  用户的最近一条微博信息字段 详细
////  status:=AJson.S['status'];
//  //allow_all_comment  boolean  是否允许所有人对我的微博进行评论，true：是，false：否
//  allow_all_comment:=AJson.B['allow_all_comment'];
//  //avatar_large  string  用户头像地址（大图），180×180像素
//  avatar_large:=AJson.S['avatar_large'];
//  //avatar_hd  string  用户头像地址（高清），高清头像原图
//  avatar_hd:=AJson.S['avatar_hd'];
//  //verified_reason  string  认证原因
//  verified_reason:=AJson.S['verified_reason'];
//  //follow_me  boolean  该用户是否关注当前登录用户，true：是，false：否
//  follow_me:=AJson.B['follow_me'];
//  //online_status  int  用户的在线状态，0：不在线、1：在线
//  online_status:=AJson.I['online_status'];
//  //bi_followers_count  int  用户的互粉数
//  bi_followers_count:=AJson.I['bi_followers_count'];
//  //lang  string  用户当前的语言版本，zh-cn：简体中文，zh-tw：繁体中文，en：英语
//  lang:=AJson.S['lang'];
//


//  Self.UserId:=AIniFile.ReadString('','UserId','');
//  Self.Password:=AIniFile.ReadString('','Password','');
//  Self.Uid:=AIniFile.ReadString('','Uid','');
//  Self.AccessToken:=AIniFile.ReadString('','AccessToken','');
//  Self.Remind_In:=AIniFile.ReadString('','Remind_In','');
//  Self.Expires_In:=AIniFile.ReadString('','Expires_In','');
//  Self.LastAuthTime:=StandardStrToDateTime(AIniFile.ReadString('','LastAuthTime',''));

  FreeAndNil(AIniFile);

  Result:=True;

end;

function Tuser.SaveToINI(AINIFilePath: String): Boolean;
var
  AIniFile:TIniFile;
begin

  Result:=False;
  AIniFile:=TIniFile.Create(AINIFilePath);

  //id  int64  用户UID
  AIniFile.WriteInteger('','id',id);
  //idstr  string  字符串型的用户UID
  AIniFile.WriteString('','idstr',idstr);
  //screen_name  string  用户昵称
  AIniFile.WriteString('','screen_name',screen_name);
  //name  string  友好显示名称
  AIniFile.WriteString('','name',name);
////  //province  int  用户所在省级ID
////  province:=AJson.I['province'];
////  //city  int  用户所在城市ID
////  city:=AJson.I['city'];
//  //location  string  用户所在地
//  location:=AJson.S['location'];
//  //description  string  用户个人描述
//  description:=AJson.S['description'];
//  //url  string  用户博客地址
//  url:=AJson.S['url'];
//  //profile_image_url  string  用户头像地址（中图），50×50像素
//  profile_image_url:=AJson.S['profile_image_url'];
//  //profile_url  string  用户的微博统一URL地址
//  profile_url:=AJson.S['profile_url'];
//  //domain  string  用户的个性化域名
//  domain:=AJson.S['domain'];
//  //weihao  string  用户的微号
//  weihao:=AJson.S['weihao'];
//  //gender  string  性别，m：男、f：女、n：未知
//  gender:=AJson.S['gender'];
  //followers_count  int  粉丝数
  AIniFile.WriteInteger('','followers_count',followers_count);
  //friends_count  int  关注数
  AIniFile.WriteInteger('','friends_count',friends_count);
  //statuses_count  int  微博数
  AIniFile.WriteInteger('','statuses_count',statuses_count);
  //favourites_count  int  收藏数
  AIniFile.WriteInteger('','favourites_count',favourites_count);
//  //created_at  string  用户创建（注册）时间
//  created_at:=AJson.S['created_at'];
//  //following  boolean  暂未支持
//  following:=AJson.B['following'];
//  //allow_all_act_msg  boolean  是否允许所有人给我发私信，true：是，false：否
//  allow_all_act_msg:=AJson.B['allow_all_act_msg'];
//  //geo_enabled  boolean  是否允许标识用户的地理位置，true：是，false：否
//  geo_enabled:=AJson.B['geo_enabled'];
//  //verified  boolean  是否是微博认证用户，即加V用户，true：是，false：否
//  verified:=AJson.B['verified'];
//  //verified_type  int  暂未支持
//  verified_type:=AJson.I['verified_type'];
//  //remark  string  用户备注信息，只有在查询用户关系时才返回此字段
//  remark:=AJson.S['remark'];
////  //status  object  用户的最近一条微博信息字段 详细
////  status:=AJson.S['status'];
//  //allow_all_comment  boolean  是否允许所有人对我的微博进行评论，true：是，false：否
//  allow_all_comment:=AJson.B['allow_all_comment'];
//  //avatar_large  string  用户头像地址（大图），180×180像素
//  avatar_large:=AJson.S['avatar_large'];
//  //avatar_hd  string  用户头像地址（高清），高清头像原图
//  avatar_hd:=AJson.S['avatar_hd'];
//  //verified_reason  string  认证原因
//  verified_reason:=AJson.S['verified_reason'];
//  //follow_me  boolean  该用户是否关注当前登录用户，true：是，false：否
//  follow_me:=AJson.B['follow_me'];
//  //online_status  int  用户的在线状态，0：不在线、1：在线
//  online_status:=AJson.I['online_status'];
//  //bi_followers_count  int  用户的互粉数
//  bi_followers_count:=AJson.I['bi_followers_count'];
//  //lang  string  用户当前的语言版本，zh-cn：简体中文，zh-tw：繁体中文，en：英语
//  lang:=AJson.S['lang'];
//



//  AIniFile.WriteString('','UserId',Self.UserId);
//  AIniFile.WriteString('','Password',Self.Password);
//  AIniFile.WriteString('','Uid',Self.Uid);
//  AIniFile.WriteString('','AccessToken',Self.AccessToken);
//  AIniFile.WriteString('','Remind_In',Self.Remind_In);
//  AIniFile.WriteString('','Expires_In',Self.Expires_In);
//  AIniFile.WriteString('','LastAuthTime',StandardDateTimeToStr(Self.LastAuthTime));

  FreeAndNil(AIniFile);
  Result:=True;

end;

function Tuser.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  //id  int64  用户UID
  id:=AJson.I['id'];
  //idstr  string  字符串型的用户UID
  idstr:=AJson.S['idstr'];
  //screen_name  string  用户昵称
  screen_name:=AJson.S['screen_name'];
  //name  string  友好显示名称
  name:=AJson.S['name'];
//  //province  int  用户所在省级ID
//  province:=AJson.I['province'];
//  //city  int  用户所在城市ID
//  city:=AJson.I['city'];
  //location  string  用户所在地
  location:=AJson.S['location'];
  //description  string  用户个人描述
  description:=AJson.S['description'];
  //url  string  用户博客地址
  url:=AJson.S['url'];
  //profile_image_url  string  用户头像地址（中图），50×50像素
  profile_image_url:=AJson.S['profile_image_url'];
  //profile_url  string  用户的微博统一URL地址
  profile_url:=AJson.S['profile_url'];
  //domain  string  用户的个性化域名
  domain:=AJson.S['domain'];
  //weihao  string  用户的微号
  weihao:=AJson.S['weihao'];
  //gender  string  性别，m：男、f：女、n：未知
  gender:=AJson.S['gender'];
  //followers_count  int  粉丝数
  followers_count:=AJson.I['followers_count'];
  //friends_count  int  关注数
  friends_count:=AJson.I['friends_count'];
  //statuses_count  int  微博数
  statuses_count:=AJson.I['statuses_count'];
  //favourites_count  int  收藏数
  favourites_count:=AJson.I['favourites_count'];
  //created_at  string  用户创建（注册）时间
  created_at:=AJson.S['created_at'];
  //following  boolean  暂未支持
  following:=AJson.B['following'];
  //allow_all_act_msg  boolean  是否允许所有人给我发私信，true：是，false：否
  allow_all_act_msg:=AJson.B['allow_all_act_msg'];
  //geo_enabled  boolean  是否允许标识用户的地理位置，true：是，false：否
  geo_enabled:=AJson.B['geo_enabled'];
  //verified  boolean  是否是微博认证用户，即加V用户，true：是，false：否
  verified:=AJson.B['verified'];
  //verified_type  int  暂未支持
  verified_type:=AJson.I['verified_type'];
  //remark  string  用户备注信息，只有在查询用户关系时才返回此字段
  remark:=AJson.S['remark'];
//  //status  object  用户的最近一条微博信息字段 详细
//  status:=AJson.S['status'];
  //allow_all_comment  boolean  是否允许所有人对我的微博进行评论，true：是，false：否
  allow_all_comment:=AJson.B['allow_all_comment'];
  //avatar_large  string  用户头像地址（大图），180×180像素
  avatar_large:=AJson.S['avatar_large'];
  //avatar_hd  string  用户头像地址（高清），高清头像原图
  avatar_hd:=AJson.S['avatar_hd'];
  //verified_reason  string  认证原因
  verified_reason:=AJson.S['verified_reason'];
  //follow_me  boolean  该用户是否关注当前登录用户，true：是，false：否
  follow_me:=AJson.B['follow_me'];
  //online_status  int  用户的在线状态，0：不在线、1：在线
  online_status:=AJson.I['online_status'];
  //bi_followers_count  int  用户的互粉数
  bi_followers_count:=AJson.I['bi_followers_count'];
  //lang  string  用户当前的语言版本，zh-cn：简体中文，zh-tw：繁体中文，en：英语
  lang:=AJson.S['lang'];





end;

{ Tgeo }

function Tgeo.ParseFromJson(AJson: ISuperObject): Boolean;
begin
//    地理信息（geo）
//    返回值字段  字段类型  字段说明
  longitude  :=AJson.S['longitude']  ;//经度坐标
  latitude  :=AJson.S['latitude']  ;//维度坐标
  city  :=AJson.S['city']  ;//所在城市的城市代码
  province  :=AJson.S['province']  ;//所在省份的省份代码
  city_name  :=AJson.S['city_name']  ;//所在城市的城市名称
  province_name  :=AJson.S['province_name']  ;//所在省份的省份名称
  address  :=AJson.S['address']  ;//所在的实际地址，可以为空
  pinyin  :=AJson.S['pinyin']  ;//地址的汉语拼音，不是所有情况都会返回该字段
  more  :=AJson.S['more']  ;//更多信息，不是所有情况都会返回该字段

end;

{ Tcomment }

destructor Tcomment.Destroy;
begin
  FreeAndNil(user);
  FreeAndNil(status);

  inherited;
end;

function Tcomment.ParseFromJson(AJson: ISuperObject): Boolean;
begin
//    created_at  :string;//  评论创建时间
//    id  :int64;//  评论的ID
//    text  :string;//  评论的内容
//    source  :string;//  评论的来源
//    user  :_object;//  评论作者的用户信息字段 详细
//    mid  :string;//  评论的MID
//    idstr  :string;//  字符串型的评论ID
//    status  :_object;//  评论的微博信息字段 详细
//    reply_comment  :_object;//  评论来源评论，当本评论属于对另一评论的回复时返回此字段

    created_at  :=AJson.S['created_at'];//  评论创建时间
    id  :=AJson.I['id'];//  评论的ID
    text  :=AJson.S['text'];//  评论的内容
    source  :=AJson.S['source'];//  评论的来源

//    user  :=AJson.S['user'];//  评论作者的用户信息字段 详细
    if AJson.Contains('user') then
    begin
      user:=Tuser.Create;
      Tuser(user).ParseFromJson(AJson.O['user']);
    end;

    mid  :=AJson.S['mid'];//  评论的MID
    idstr  :=AJson.S['idstr'];//  字符串型的评论ID
//    status  :=AJson.S['pinyin'];//  评论的微博信息字段 详细
    if AJson.Contains('status') then
    begin
      status:=Tstatus.Create;
      Tstatus(status).ParseFromJson(AJson.O['status']);
    end;

//    reply_comment  :=AJson.S['pinyin'];//  评论来源评论，当本评论属于对另一评论的回复时返回此字段

end;

{ TTcomment }

function Tcomments.GetItem(Index: Integer): Tcomment;
begin
  Result:=Tcomment(Inherited Items[Index]);
end;





{ Tuser_count }

function Tuser_count.ParseFromJson(AJson: ISuperObject): Boolean;
begin
  //id  int64  用户UID
  id:=AJson.I['id'];
  //followers_count  int  粉丝数
  followers_count:=AJson.I['followers_count'];
  //friends_count  int  关注数
  friends_count:=AJson.I['friends_count'];
  //statuses_count  int  微博数
  statuses_count:=AJson.I['statuses_count'];


end;

{ Tuser_counts }

function Tuser_counts.GetItem(Index: Integer): Tuser_count;
begin
  Result:=Tuser_count(Inherited Items[Index]);
end;

end.
