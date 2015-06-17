unit LoginFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl,
  uSkinFireMonkeyButton,
  uSkinFireMonkeyImage,
  uSkinFireMonkeyLabel,
  Math,
//  uMobileUtils,
  uUIFunction, uSkinFireMonkeyPanel, uSkinFireMonkeyFrameImage, FMX.Edit,
  uSkinFireMonkeyEdit, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uSkinFireMonkeyListView, uSkinAnimator, FMX.Controls.Presentation,
  uSkinImageList, uDrawPicture, uSkinFireMonkeyDirectUIParent,
  uSkinFireMonkeyScrollBoxContent, uSkinScrollBoxContentType,
  uSkinFireMonkeyScrollBox, uSkinFireMonkeyVirtualList;

type
  TFrameLogin = class(TFrame,IFrameVirtualKeyboardEvent)
    imgBackground: TSkinFMXImage;
    imglistHead: TSkinImageList;
    pnlVirtualKeyboard: TSkinFMXPanel;
    sbClient: TSkinFMXScrollBox;
    sbcClient: TSkinFMXScrollBoxContent;
    imgHead: TSkinFMXFrameImage;
    cmaExpand: TSkinControlMoveAnimator;
    lvLoginUsers: TSkinFMXListView;
    pnlLogin: TSkinFMXPanel;
    btnLogin: TSkinFMXButton;
    edtPass: TSkinFMXEdit;
    imgDevide3: TSkinFMXImage;
    imgDevide4: TSkinFMXImage;
    edtUser: TSkinFMXEdit;
    btnExpand: TSkinFMXButton;
    imgDevide1: TSkinFMXImage;
    imgDevide2: TSkinFMXImage;
    lblHelp: TSkinFMXLabel;
    btnNewUser: TSkinFMXButton;
    procedure imgBackgroundResize(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
  private
    //显示虚拟键盘
    procedure DoVirtualKeyboardShow(KeyboardVisible: Boolean; const Bounds: TRect);
    //隐藏虚拟键盘
    procedure DoVirtualKeyboardHide(KeyboardVisible: Boolean; const Bounds: TRect);
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
  public
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalLoginFrame:TFrameLogin;

implementation

{$R *.fmx}

uses
  MainForm;

{ TFrameLogin }

procedure TFrameLogin.DoVirtualKeyboardHide(KeyboardVisible: Boolean;const Bounds: TRect);
begin
  Self.pnlVirtualKeyboard.Height:=0;
end;

procedure TFrameLogin.DoVirtualKeyboardShow(KeyboardVisible: Boolean;const Bounds: TRect);
var
  AFixTop:Double;
begin
  if Bounds.Height-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight>Self.pnlVirtualKeyboard.Height then
  begin
    Self.pnlVirtualKeyboard.Height:=RectHeight(Bounds)-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight;
    //下面要相等
    //Self.edtPass.Top+Self.edtPass.Height=Self.pnlVirtualKeyboard.Top;
    //计算出合理的位置
    AFixTop:=Self.pnlLogin.Top+Self.edtPass.Top+Self.edtPass.Height-Self.pnlVirtualKeyboard.Top;
    Self.sbClient.VertScrollBar.Properties.Position:=AFixTop;
  end;
end;

procedure TFrameLogin.OnReturnFrame(FromFrame: TFrame);
begin
end;

procedure TFrameLogin.btnLoginClick(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

constructor TFrameLogin.Create(AOwner: TComponent);
begin
  inherited;
  Self.pnlVirtualKeyboard.Height:=0;

  Self.lvLoginUsers.Visible:=False;
//  Self.sbcClient.HeightInt:=frmMain.Height;
end;

procedure TFrameLogin.btnExpandClick(Sender: TObject);
begin
  if Self.btnExpand.Properties.IsPushed then
  begin
    Self.lvLoginUsers.Visible:=True;
    Self.cmaExpand.DirectionType:=adtForward;
    Self.cmaExpand.Start;
  end
  else
  begin
    Self.lvLoginUsers.Visible:=False;
    Self.cmaExpand.DirectionType:=adtBackward;
    Self.cmaExpand.Start;
  end;
end;

procedure TFrameLogin.imgBackgroundResize(Sender: TObject);
begin
  Self.imgHead.Left:=Ceil(Width-Self.imgHead.Width) div 2;
//  Self.SkinFMXEdit1.Position.X:=Ceil(Width-Self.SkinFMXEdit1.Width) div 2;
//  Self.SkinFMXEdit2.Position.X:=Ceil(Width-Self.SkinFMXEdit2.Width) div 2;
end;

end.
