unit MobileLoginFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl, uSkinFireMonkeyImage, FMX.Controls.Presentation,
  uUIFunction,
  Math,
  MobileAuthFrame,
  FMX.Edit, uSkinFireMonkeyEdit, uSkinFireMonkeyButton, uSkinFireMonkeyLabel,
  uSkinFireMonkeyPanel, uDrawPicture, uSkinImageList;

type
  TFrameMobileLogin = class(TFrame)
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXImage2: TSkinFMXImage;
    SkinFMXEdit2: TSkinFMXEdit;
    SkinFMXEdit1: TSkinFMXEdit;
    SkinFMXImage3: TSkinFMXImage;
    SkinFMXImage4: TSkinFMXImage;
    ClearEditButton1: TClearEditButton;
    ClearEditButton2: TClearEditButton;
    btnLogin: TSkinFMXButton;
    lblHelp: TSkinFMXLabel;
    SkinFMXLabel1: TSkinFMXLabel;
    pnlToolBar: TSkinFMXPanel;
    SkinFMXImage5: TSkinFMXImage;
    SkinImageList1: TSkinImageList;
    imgToolBarDevide: TSkinFMXImage;
    SkinFMXLabel2: TSkinFMXLabel;
    procedure btnLoginClick(Sender: TObject);
    procedure SkinFMXImage5Click(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SkinFMXLabel2Click(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    { Public declarations }
  end;



var
  GlobalMobileLoginFrame:TFrameMobileLogin;


implementation

uses
  MobileMainForm;

{$R *.fmx}

procedure TFrameMobileLogin.btnLoginClick(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameMobileLogin.FrameResize(Sender: TObject);
begin
  Self.SkinFMXLabel1.Left:=Ceil(Self.Width-Self.SkinFMXLabel1.WidthInt) div 2;
  Self.SkinFMXImage1.Left:=Ceil(Self.Width-Self.SkinFMXImage1.WidthInt) div 2;
end;

procedure TFrameMobileLogin.SkinFMXImage5Click(Sender: TObject);
begin
  FrameHistroy.OnReturnFrame:=nil;
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameMobileLogin.SkinFMXLabel2Click(Sender: TObject);
begin
  HideFrame(Self);

  //授权
  ShowFrame(TFrame(GlobalMobileAuthFrame),TFrameMobileAuth,frmMobileMain,nil,nil,frmMobileMain.OnReturnFrameAfterAuth,Application);
  GlobalMobileAuthFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalMobileAuthFrame.btnAuthClick(Self);

end;

end.
