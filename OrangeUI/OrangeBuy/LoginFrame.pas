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
  uUIFunction, uSkinFireMonkeyPanel, uSkinFireMonkeyFrameImage, FMX.Edit,
  uSkinFireMonkeyEdit, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uSkinFireMonkeyListView, uSkinAnimator, FMX.Controls.Presentation,
  uSkinImageList, uDrawPicture, uSkinFireMonkeySwitch;

type
  TFrameLogin = class(TFrame)
    pnlUser: TSkinFMXPanel;
    imgDevide2: TSkinFMXImage;
    btnExpand: TSkinFMXButton;
    edtUser: TSkinFMXEdit;
    pnlPass: TSkinFMXPanel;
    imgDevide4: TSkinFMXImage;
    edtPass: TSkinFMXEdit;
    btnLogin: TSkinFMXButton;
    lblHelp: TSkinFMXLabel;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXImage2: TSkinFMXImage;
    pnlToolBar1: TSkinFMXPanel;
    SkinFMXPanel1: TSkinFMXPanel;
    btnReturn: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXSwitch1: TSkinFMXSwitch;
    procedure imgBackgroundResize(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure SkinFMXSwitch1Click(Sender: TObject);
  private
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

{ TFrameLogin }

procedure TFrameLogin.OnReturnFrame(FromFrame: TFrame);
begin
end;

procedure TFrameLogin.btnReturnClick(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

procedure TFrameLogin.SkinFMXSwitch1Click(Sender: TObject);
begin
  Self.edtPass.Password:=not Self.SkinFMXSwitch1.Properties.Checked;
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
//  Self.lvLoginUsers.Visible:=False;
end;

procedure TFrameLogin.btnExpandClick(Sender: TObject);
begin
//  if Self.btnExpand.Properties.IsPushed then
//  begin
//    Self.lvLoginUsers.Visible:=True;
//    Self.cmaExpand.DirectionType:=adtForward;
//    Self.cmaExpand.Start;
//  end
//  else
//  begin
//    Self.lvLoginUsers.Visible:=False;
//    Self.cmaExpand.DirectionType:=adtBackward;
//    Self.cmaExpand.Start;
//  end;
end;

procedure TFrameLogin.imgBackgroundResize(Sender: TObject);
begin
//  Self.imgHead.Left:=Ceil(Width-Self.imgHead.Width) div 2;
//  Self.SkinFMXEdit1.Position.X:=Ceil(Width-Self.SkinFMXEdit1.Width) div 2;
//  Self.SkinFMXEdit2.Position.X:=Ceil(Width-Self.SkinFMXEdit2.Width) div 2;
end;

end.
