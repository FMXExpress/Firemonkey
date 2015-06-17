unit ProductInfoFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinImageList, uSkinFireMonkeyImage, uSkinFireMonkeyLabel,
  uUIFunction,
  BuyProductFrame,
  uSkinFireMonkeyPanel, uSkinFireMonkeyButton, uSkinButtonType,
  uSkinFireMonkeyImageListViewer, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyControl, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uDrawPicture, uSkinMaterial, uSkinFireMonkeyNotifyNumberIcon;

type
  TFrameProductInfo = class(TFrame)
    lbHome: TSkinFMXListBox;
    ItemHeader: TSkinFMXItemDesignerPanel;
    imgPlayer: TSkinFMXImageListViewer;
    ItemSearchBar: TSkinFMXItemDesignerPanel;
    lblDefaultPrice: TSkinFMXLabel;
    tmrLoading: TTimer;
    imglistPlayer: TSkinImageList;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXLabel1: TSkinFMXLabel;
    ItemItem1: TSkinFMXItemDesignerPanel;
    ItemItem3: TSkinFMXItemDesignerPanel;
    bgIndicator: TSkinFMXButtonGroup;
    btnmItem1: TSkinButtonDefaultMaterial;
    btnPublic: TSkinFMXButton;
    btnGroup: TSkinFMXButton;
    SkinFMXImage3: TSkinFMXImage;
    imglistItem1: TSkinImageList;
    SkinFMXLabel5: TSkinFMXLabel;
    SkinFMXLabel9: TSkinFMXLabel;
    SkinFMXLabel10: TSkinFMXLabel;
    SkinFMXLabel11: TSkinFMXLabel;
    SkinFMXLabel12: TSkinFMXLabel;
    SkinFMXLabel13: TSkinFMXLabel;
    SkinFMXLabel14: TSkinFMXLabel;
    SkinFMXLabel2: TSkinFMXLabel;
    SkinFMXButton3: TSkinFMXButton;
    SkinFMXButton8: TSkinFMXButton;
    SkinFMXLabel3: TSkinFMXLabel;
    SkinFMXButton9: TSkinFMXButton;
    SkinFMXButton10: TSkinFMXButton;
    SkinFMXButton11: TSkinFMXButton;
    ItemMenu: TSkinFMXItemDesignerPanel;
    lblMenuCaption: TSkinFMXLabel;
    imgExpandState: TSkinFMXImage;
    lblMenuDetail: TSkinFMXLabel;
    SkinFMXLabel4: TSkinFMXLabel;
    SkinFMXLabel6: TSkinFMXLabel;
    SkinFMXLabel7: TSkinFMXLabel;
    imgBuy: TSkinFMXImage;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    SkinFMXButton7: TSkinFMXButton;
    nniCartCount: TSkinFMXNotifyNumberIcon;
    procedure SkinFMXButton1Click(Sender: TObject);
    procedure SkinFMXButton6Click(Sender: TObject);
    procedure SkinFMXButton5Click(Sender: TObject);
    procedure SkinFMXButton10Click(Sender: TObject);
    procedure SkinFMXButton11Click(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;


var
  GlobalProductInfoFrame:TFrameProductInfo;


implementation



{$R *.fmx}


uses
  MainForm,MainFrame;


{ TFrameProductInfo }

constructor TFrameProductInfo.Create(AOwner: TComponent);
begin
  inherited;
//  Self.lbHome.Properties.Items.FindItemByCaption('图片').Height:=1000;

  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

end;

procedure TFrameProductInfo.SkinFMXButton10Click(Sender: TObject);
begin
  if StrToInt(Self.SkinFMXButton9.Text)>1 then
  begin
    Self.SkinFMXButton9.Text:=IntToStr(StrToInt(Self.SkinFMXButton9.Text)-1);
  end;

end;

procedure TFrameProductInfo.SkinFMXButton11Click(Sender: TObject);
begin
//  if StrToInt(Self.SkinFMXButton9.Text)>=0 then
//  begin
    Self.SkinFMXButton9.Text:=IntToStr(StrToInt(Self.SkinFMXButton9.Text)+1);
//  end;

end;

procedure TFrameProductInfo.SkinFMXButton1Click(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

procedure TFrameProductInfo.SkinFMXButton5Click(Sender: TObject);
begin
//    //查看商品信息
//    HideFrame(Self);

//  //显示购买产品界面
//  ShowFrame(TFrame(GlobalBuyProductFrame),TFrameBuyProduct,frmMain,nil,nil,nil,Application);
//  GlobalBuyProductFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameProductInfo.SkinFMXButton6Click(Sender: TObject);
begin
//    //查看商品信息
//    HideFrame(Self);

//  //显示购买产品界面
//  ShowFrame(TFrame(GlobalBuyProductFrame),TFrameBuyProduct,frmMain,nil,nil,nil,Application);
//  GlobalBuyProductFrame.FrameHistroy:=CurrentFrameHistroy;

end;

end.
