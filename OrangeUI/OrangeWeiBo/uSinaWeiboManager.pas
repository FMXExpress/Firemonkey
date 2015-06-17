unit uSinaWeiboManager;

interface

uses
//  Windows,
  Classes,
  SysUtils,
  uPublic,
//  Forms,
  uFuncCommon,
  IniFiles,
  uTimerTask,
  uFileCommon,
  uIdHttpControl,
  uOpenPlatform,
  uDataStructure,
  uAPIItem_users_show,
  uAPIItem_statuses_update,
  uAPIItem_statuses_home_timeline,
  uAPIItem_statuses_public_timeline;


type
  TLoginStep=(
              asNone,
              asAuthPage,//授权界面
              asSubmitPage//提交界面
              );

  TLoginType=(
              atAuto,//自动授权
              atManual//用户手动授权
              );

  TSinaWeiboManager=class
  private
  public
    App:TApp;
    OAuth2User:TOAuth2User;
    Current_user:Tuser;
  public
    constructor Create;
    destructor Destroy;override;
  public
    //获取用户信息
    Getusers_show_APIResponse:TAPIResponse_users_show;
    Getusers_show_uid:String;
    procedure Asyc_Getusers_show(uid:String;OnExecuteEndEvent:TTaskNotify);
    procedure DoGetusers_show_InThread(ATimerTask:TObject);

  public
    //获取微博
    Gethome_timeline_APIResponse:TAPIResponse_statuses_home_timeline;
    Gethome_timeline_PageIndex:String;
    Gethome_timeline_Since_Id:String;
    Gethome_timeline_Max_Id:String;
    procedure Async_Gethome_timeline(PageIndex: String;
                                     Since_Id:String;
                                     Max_Id:String;
                                     OnExecuteEndEvent:TTaskNotify);
    procedure DoGethome_timeline_InThread(ATimerTask:TObject);
  end;


var
  GlobalManager:TSinaWeiboManager;
  GlobalOpenPlatform:TOpenPlatform;
  GlobalHttpControl:TIdHttpControl;

implementation


{ TSinaWeiboManager }

procedure TSinaWeiboManager.Asyc_Getusers_show(uid: String;
  OnExecuteEndEvent: TTaskNotify);
var
  ATimerTask:TTimerTask;
begin
  Getusers_show_uid:=uid;

  ATimerTask:=TTimerTask.Create(0);
  ATimerTask.OnExecute:=Self.DoGetusers_show_InThread;
  ATimerTask.OnExecuteEnd:=OnExecuteEndEvent;
  GetGlobalTimerThread.RunTask(ATimerTask);

end;

procedure TSinaWeiboManager.Async_Gethome_timeline(PageIndex: String;
                                                  Since_Id:String;
                                                 Max_Id:String;
                                                 OnExecuteEndEvent:TTaskNotify);
var
  ATimerTask:TTimerTask;
begin
  Gethome_timeline_PageIndex:=PageIndex;
  Gethome_timeline_Since_Id:=Since_Id;
  Gethome_timeline_Max_Id:=Max_Id;

  ATimerTask:=TTimerTask.Create(0);
  ATimerTask.OnExecute:=DoGethome_timeline_InThread;
  ATimerTask.OnExecuteEnd:=OnExecuteEndEvent;
  GetGlobalTimerThread.RunTask(ATimerTask);
end;

constructor TSinaWeiboManager.Create;
begin
  Current_user:=Tuser.Create;
  if FileExists(uFileCommon.GetApplicationPath+'CurrentUser.ini') then
  begin
    Current_user.LoadFromINI(uFileCommon.GetApplicationPath+'CurrentUser.ini');
  end;


  App:=TApp.Create;
  App.AppKey:='1418225871';
  App.AppSecret:='a14b358473326e222142cd958b3cd6be';
  App.CallBackUrl:='https://api.weibo.com/oauth2/default.html';
//  App.CallBackUrl:='http://blog.csdn.net/delphiteacher';
//  App.LoadFromXML(uFileCommon.GetApplicationPath+'App.xml');
//  App.LoadFromINI(uFileCommon.GetApplicationPath+'App.ini');
//  App.SaveToINI(uFileCommon.GetApplicationPath+'App.ini');


  OAuth2User:=TOAuth2User.Create;
  //加载用户及授权信息
  OAuth2User.UserId:='ggggcexx@163.com';
  OAuth2User.Password:='';
  OAuth2User.AccessToken:='2.00H8jjnBp1jyXB72db5522e10zv6ta';
  OAuth2User.Remind_In:='157679999';
  OAuth2User.Expires_In:='157679999';
  OAuth2User.Uid:='1651072717';
  OAuth2User.LastAuthTime:=StandardStrToDateTime('2014-07-11 09:56:40');
//  OAuth2User.LoadFromXML(uFileCommon.GetApplicationPath+'OAuth2User.xml');
//  OAuth2User.LoadFromINI(uFileCommon.GetApplicationPath+'OAuth2User.ini');
//  OAuth2User.SaveToINI(uFileCommon.GetApplicationPath+'OAuth2User.ini');

end;

destructor TSinaWeiboManager.Destroy;
begin

  FreeAndNil(Current_user);

  FreeAndNil(OAuth2User);
  FreeAndNil(App);

  FreeAndNil(Getusers_show_APIResponse);
  FreeAndNil(Gethome_timeline_APIResponse);

  inherited;
end;

procedure TSinaWeiboManager.DoGethome_timeline_InThread(ATimerTask:TObject);
var
  APIItem:TAPIItem;
begin
  //创建接口
  APIItem:=CreateAPIItem('statuses/home_timeline');
  //创建返回
  Gethome_timeline_APIResponse:=TAPIResponse_statuses_home_timeline.Create;
  //设置参数
  APIItem.ParamList.SetValue('access_token',GlobalManager.OAuth2User.AccessToken);
  if Gethome_timeline_PageIndex<>'' then
  begin
    APIItem.ParamList.SetValue('page',Gethome_timeline_PageIndex);
  end;
  if Gethome_timeline_Since_Id<>'' then
  begin
    APIItem.ParamList.SetValue('since_id',Gethome_timeline_Since_Id);
  end;
  if Gethome_timeline_Max_Id<>'' then
  begin
    APIItem.ParamList.SetValue('max_id',Gethome_timeline_Max_Id);
  end;


  //调用接口
  try
    GlobalOpenPlatform.CallAPI(GlobalManager.App,
                GlobalManager.OAuth2User,
                APIItem,
                GlobalHttpControl,
                Gethome_timeline_APIResponse);
  except

  end;


  //释放API
  FreeAndNil(APIItem);

end;

procedure TSinaWeiboManager.DoGetusers_show_InThread(ATimerTask: TObject);
var
  APIItem:TAPIItem;
begin
  //创建接口
  APIItem:=CreateAPIItem('users/show');
  //创建返回
  FreeAndNil(Getusers_show_APIResponse);
  Getusers_show_APIResponse:=TAPIResponse_users_show.Create;
  //设置参数
  APIItem.ParamList.SetValue('access_token',GlobalManager.OAuth2User.AccessToken);
  if Getusers_show_uid<>'' then
  begin
    APIItem.ParamList.SetValue('uid',Getusers_show_uid);
  end;


  //调用接口
  try
    if GlobalOpenPlatform.CallAPI(GlobalManager.App,
                GlobalManager.OAuth2User,
                APIItem,
                GlobalHttpControl,
                Getusers_show_APIResponse) then
    begin
      FreeAndNil(Current_user);
      Self.Current_user:=Getusers_show_APIResponse.user;
      Getusers_show_APIResponse.user:=nil;
      Current_user.SaveToINI(uFileCommon.GetApplicationPath+'CurrentUser.ini');
    end;
  except

  end;


  //释放API
  FreeAndNil(APIItem);

end;

initialization
  GlobalManager:=TSinaWeiboManager.Create;
  GlobalOpenPlatform:=TOpenPlatform.Create;
  GlobalHttpControl:=TIdHttpControl.Create;

finalization
  FreeAndNil(GlobalOpenPlatform);
  FreeAndNil(GlobalHttpControl);
  FreeAndNil(GlobalManager);

end.
