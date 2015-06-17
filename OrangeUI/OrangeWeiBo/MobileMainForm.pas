unit MobileMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox,
  MobileMainFrame,
  MobileLoginFrame,
  MobileAuthFrame,
//  MobileProfileFrame,
//  MobileSelPublishTypeFrame,
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
  uSkinFireMonkeyButton;

type
  TfrmMobileMain = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OnReturnFrameAfterLogin(FromFrame:TFrame);
    procedure OnReturnFrameAfterAuth(FromFrame:TFrame);
    procedure ShowMainFrame;
    { Public declarations }
  end;

var
  frmMobileMain: TfrmMobileMain;

implementation

{$R *.fmx}

procedure TfrmMobileMain.FormShow(Sender: TObject);
begin

  //登录
  ShowFrame(TFrame(GlobalMobileLoginFrame),TFrameMobileLogin,Self,nil,nil,OnReturnFrameAfterLogin,Application);
  GlobalMobileLoginFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TfrmMobileMain.OnReturnFrameAfterAuth(FromFrame: TFrame);
begin
  ShowMainFrame;
end;

procedure TfrmMobileMain.OnReturnFrameAfterLogin(FromFrame: TFrame);
begin
  if Now>GlobalManager.OAuth2User.ExpiresTime then
  begin
    //授权码已到期，重新授权
    ShowFrame(TFrame(GlobalMobileAuthFrame),TFrameMobileAuth,Self,nil,nil,OnReturnFrameAfterAuth,Application);
    GlobalMobileAuthFrame.FrameHistroy:=CurrentFrameHistroy;
    GlobalMobileAuthFrame.btnAuthClick(Self);
  end
  else
  begin
    ShowMainFrame;
  end;
end;

procedure TfrmMobileMain.ShowMainFrame;
begin
  //显示主界面
  ShowFrame(TFrame(GlobalMobileMainFrame),TFrameMobileMain,Self,nil,nil,nil,Application);
  GlobalMobileMainFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalMobileMainFrame.Show;
end;

end.
