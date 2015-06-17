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
  uDrawPicture;

type
  TFrameProductInfo = class(TFrame)
    lbHome: TSkinFMXListBox;
    ItemHeader: TSkinFMXItemDesignerPanel;
    imgPlayer: TSkinFMXImageListViewer;
    ItemFooter: TSkinFMXItemDesignerPanel;
    lblFooterCaption: TSkinFMXLabel;
    ItemDefault: TSkinFMXItemDesignerPanel;
    lblDefaultPrice: TSkinFMXLabel;
    tmrLoading: TTimer;
    imglistPlayer: TSkinImageList;
    imglistLeft: TSkinImageList;
    imglistRight: TSkinImageList;
    imglistProduct: TSkinImageList;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXButton3: TSkinFMXButton;
    ItemWebLink: TSkinFMXItemDesignerPanel;
    ItemItem1: TSkinFMXItemDesignerPanel;
    pnlWaitPay: TSkinFMXPanel;
    lblWaitPayCount: TSkinFMXLabel;
    lblWaitPay: TSkinFMXLabel;
    pnlWaitRecv: TSkinFMXPanel;
    lblWaitRecvCount: TSkinFMXLabel;
    lblWaitRecv: TSkinFMXLabel;
    SkinFMXPanel3: TSkinFMXPanel;
    SkinFMXLabel2: TSkinFMXLabel;
    SkinFMXLabel3: TSkinFMXLabel;
    ItemItem2: TSkinFMXItemDesignerPanel;
    SkinFMXPanel6: TSkinFMXPanel;
    SkinFMXLabel6: TSkinFMXLabel;
    SkinFMXLabel7: TSkinFMXLabel;
    SkinFMXPanel7: TSkinFMXPanel;
    btnLoadNextData: TSkinFMXButton;
    SkinFMXButton4: TSkinFMXButton;
    ItemItem3: TSkinFMXItemDesignerPanel;
    SkinFMXLabel8: TSkinFMXLabel;
    ItemItem4: TSkinFMXItemDesignerPanel;
    SkinFMXImage2: TSkinFMXImage;
    imgBuy: TSkinFMXImage;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    SkinFMXButton7: TSkinFMXButton;
    SkinFMXLabel4: TSkinFMXLabel;
    SkinFMXPanel2: TSkinFMXPanel;
    procedure SkinFMXButton1Click(Sender: TObject);
    procedure SkinFMXButton6Click(Sender: TObject);
    procedure SkinFMXButton5Click(Sender: TObject);
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
  Self.lbHome.Properties.Items.FindItemByCaption('图片').Height:=1000;

  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

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

  //显示购买产品界面
  ShowFrame(TFrame(GlobalBuyProductFrame),TFrameBuyProduct,frmMain,nil,nil,nil,Application);
  GlobalBuyProductFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameProductInfo.SkinFMXButton6Click(Sender: TObject);
begin
//    //查看商品信息
//    HideFrame(Self);

  //显示购买产品界面
  ShowFrame(TFrame(GlobalBuyProductFrame),TFrameBuyProduct,frmMain,nil,nil,nil,Application);
  GlobalBuyProductFrame.FrameHistroy:=CurrentFrameHistroy;

end;

end.
