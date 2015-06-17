unit MobileMainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox,
  MobileHomeFrame,
  MobileMyFrame,
  MobileFindFrame,
  MobileMsgFrame,
  MobileSelPublishTypeFrame,
//  uOpenPlatform,
//  uDataStructure,
  uSinaWeiboManager,
  uTimerTask,
  uUIFunction,
//  uAPIItem_statuses_update,
//  uAPIItem_statuses_home_timeline,
//  uAPIItem_statuses_public_timeline,
  uSkinFireMonkeyControl,
  uSkinFireMonkeyPanel,
  uSkinFireMonkeyImage,
  uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox,
  uSkinFireMonkeyButton, uSkinFireMonkeySwitchPageListPanel,
  uSkinPageControlType, uSkinFireMonkeyPageControl;

type
  TFrameMobileMain = class(TFrame)
    imgBtnBackGround: TSkinFMXImage;
    btnHome: TSkinFMXButton;
    btnMy: TSkinFMXButton;
    btnFind: TSkinFMXButton;
    btnMsg: TSkinFMXButton;
    btnAdd: TSkinFMXButton;
    pcHome: TSkinFMXPageControl;
    tsHome: TSkinFMXTabSheet;
    tsMsg: TSkinFMXTabSheet;
    tsFind: TSkinFMXTabSheet;
    tsMy: TSkinFMXTabSheet;
    procedure FormResize(Sender: TObject);
    procedure btnHomeClick(Sender: TObject);
    procedure btnMyClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnMsgClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
  private
    procedure OnAsync_Getusers_show_ExecuteEnd(ATimerTask:TObject);
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure Show;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalMobileMainFrame: TFrameMobileMain;

implementation

uses
  MobileMainForm;

{$R *.fmx}

procedure TFrameMobileMain.btnAddClick(Sender: TObject);
begin
  HideFrame(Self);

  //显示选择发布类型界面
  ShowFrame(TFrame(GlobalMobileSelPublishTypeFrame),TFrameMobileSelPublishType,frmMobileMain,nil,nil,nil,Application);
  GlobalMobileSelPublishTypeFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalMobileSelPublishTypeFrame.ShowEnterAnimation;
end;

procedure TFrameMobileMain.btnFindClick(Sender: TObject);
begin
  //显示发现界面
  ShowFrame(TFrame(GlobalMobileFindFrame),TFrameMobileFind,tsFind,nil,nil,nil,Application,False);
  Self.pcHome.Properties.ActivePage:=tsFind;
end;

procedure TFrameMobileMain.btnHomeClick(Sender: TObject);
begin
  //显示首页界面
  ShowFrame(TFrame(GlobalMobileHomeFrame),TFrameMobileHome,tsHome,nil,nil,nil,Application,False);
  Self.pcHome.Properties.ActivePage:=tsHome;
end;

procedure TFrameMobileMain.btnMsgClick(Sender: TObject);
begin
  //显示消息界面
  ShowFrame(TFrame(GlobalMobileMsgFrame),TFrameMobileMsg,tsMsg,nil,nil,nil,Application,False);
  Self.pcHome.Properties.ActivePage:=tsMsg;
end;

procedure TFrameMobileMain.btnMyClick(Sender: TObject);
begin
  //显示设置界面
  ShowFrame(TFrame(GlobalMobileMyFrame),TFrameMobileMy,tsMy,nil,nil,nil,Application,False);
  Self.pcHome.Properties.ActivePage:=tsMy;
  GlobalMobileMyFrame.Sync;
end;

constructor TFrameMobileMain.Create(AOwner: TComponent);
begin
  inherited;
  Self.pcHome.Properties.Orientation:=TTabOrientation.toNone;
  btnHomeClick(nil);
end;

procedure TFrameMobileMain.FormResize(Sender: TObject);
begin
  Self.btnHome.Width:=(Width-Self.btnAdd.Width) / 4;
  Self.btnMsg.Width:=(Width-Self.btnAdd.Width) / 4;
  Self.btnFind.Width:=(Width-Self.btnAdd.Width) / 4;
  Self.btnMy.Width:=(Width-Self.btnAdd.Width) / 4;
end;

procedure TFrameMobileMain.Show;
begin
  //获取用户信息
  GlobalManager.Asyc_Getusers_show(GlobalManager.OAuth2User.Uid,Self.OnAsync_Getusers_show_ExecuteEnd);
end;

procedure TFrameMobileMain.OnAsync_Getusers_show_ExecuteEnd(ATimerTask: TObject);
begin
  //加载用户信息
  if GlobalMobileHomeFrame<>nil then
  begin
    GlobalMobileHomeFrame.OnAsync_Getusers_show_ExecuteEnd(ATimerTask);
  end;
end;

end.
