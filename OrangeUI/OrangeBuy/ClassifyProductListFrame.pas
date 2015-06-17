unit ClassifyProductListFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  uUIFunction,
  uSkinItems,
  uSkinListBoxType,
  uSkinListViewType,
  ProductInfoFrame,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyButton,
  uSkinImageList, uSkinFireMonkeyLabel, uSkinButtonType,
  uSkinFireMonkeyImageListViewer, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinFireMonkeyImage,
  uDrawPicture, uSkinMaterial, uSkinFireMonkeyListView, uSkinFireMonkeyCheckBox,
  uSkinFireMonkeyPullLoadPanel, uSkinPullLoadPanelType,
  uSkinFireMonkeyVirtualList;

type
  TFrameClassifyProductList = class(TFrame)
    tmrLoading: TTimer;
    imglistProduct: TSkinImageList;
    SkinFMXPanel1: TSkinFMXPanel;
    btnSortByDefault: TSkinFMXButton;
    btnSortBySellCount: TSkinFMXButton;
    btnSortByPrice: TSkinFMXButton;
    pnlToolBar1: TSkinFMXPanel;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    btnSortByGood: TSkinFMXButton;
    btmColorButton: TSkinButtonDefaultMaterial;
    lbProductList: TSkinFMXListView;
    ListItemDefault: TSkinFMXItemDesignerPanel;
    imgListItemDefaultIcon: TSkinFMXImage;
    lblListItemDefaultCaption: TSkinFMXLabel;
    lblListItemDefaultPrice: TSkinFMXLabel;
    SkinFMXPanel3: TSkinFMXPanel;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    bdmBottomButton: TSkinButtonDefaultMaterial;
    SkinFMXPanel5: TSkinFMXPanel;
    lblListItemDefaultDetail1: TSkinFMXLabel;
    lblListItemDefaultDetail2: TSkinFMXLabel;
    imgListItemDefaultBuy: TSkinFMXImage;
    SkinFMXPanel4: TSkinFMXPanel;
    ViewItemDefault: TSkinFMXItemDesignerPanel;
    imgViewItemDefaultIcon: TSkinFMXImage;
    lblViewItemDefaultPrice: TSkinFMXLabel;
    lblViewItemDefaultDetail1: TSkinFMXLabel;
    imgViewItemDefaultBuy: TSkinFMXImage;
    plpTop: TSkinFMXPullLoadPanel;
    lblTopLoading: TSkinFMXLabel;
    imgTopLoading: TSkinFMXImage;
    imgTopLoadHint: TSkinFMXImage;
    procedure btnReturnClick(Sender: TObject);
    procedure btnLoadNextDataClick(Sender: TObject);
    procedure tmrLoadingTimer(Sender: TObject);
    procedure lbProductListClickItem(Sender: TObject);
    procedure SkinFMXPanel1Resize(Sender: TObject);
    procedure SkinFMXButton1Click(Sender: TObject);
    procedure imgListItemDefaultBuyClick(Sender: TObject);
    procedure SkinFMXButton5Click(Sender: TObject);
    procedure plpTopExecuteLoad(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;


var
  GlobalClassifyProductListFrame:TFrameClassifyProductList;

implementation

{$R *.fmx}

uses
  MainForm,MainFrame;

procedure TFrameClassifyProductList.btnLoadNextDataClick(Sender: TObject);
begin
  //正在加载
  if not tmrLoading.Enabled then
  begin
    tmrLoading.Enabled:=True;
//    btnLoadNextData.Caption:='正在载入...';
  end;
end;

procedure TFrameClassifyProductList.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

constructor TFrameClassifyProductList.Create(AOwner: TComponent);
begin
  inherited;

  SkinFMXButton5Click(Self);

  tmrLoadingTimer(Self);

end;

procedure TFrameClassifyProductList.lbProductListClickItem(Sender: TObject);
begin
  if TSkinItem(Sender).ItemType=sitDefault then
  begin
    //查看商品信息
    HideFrame(Self);

    //显示产品信息界面
    ShowFrame(TFrame(GlobalProductInfoFrame),TFrameProductInfo,frmMain,nil,nil,nil,Application);
    GlobalProductInfoFrame.FrameHistroy:=CurrentFrameHistroy;

  end;
end;

procedure TFrameClassifyProductList.plpTopExecuteLoad(Sender: TObject);
begin
  Self.tmrLoading.Enabled:=True;
end;

procedure TFrameClassifyProductList.SkinFMXButton1Click(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameClassifyProductList.SkinFMXButton5Click(Sender: TObject);
begin
  if SkinFMXButton5.Properties.IsPushed then
  begin
    Self.lbProductList.Properties.ItemDesignerPanel:=nil;
    Self.lbProductList.Properties.ViewType:=lvtIcon;
    Self.lbProductList.Properties.ItemWidth:=160;
    Self.lbProductList.Properties.ItemHeight:=200;
    Self.lbProductList.Properties.ItemDesignerPanel:=Self.ViewItemDefault;
  end
  else
  begin
    Self.lbProductList.Properties.ItemDesignerPanel:=nil;
    Self.lbProductList.Properties.ViewType:=lvtList;
    Self.lbProductList.Properties.ItemWidth:=-1;
    Self.lbProductList.Properties.ItemHeight:=100;
    Self.lbProductList.Properties.ItemDesignerPanel:=Self.ListItemDefault;
  end;
end;

procedure TFrameClassifyProductList.imgListItemDefaultBuyClick(Sender: TObject);
begin
  //

end;

procedure TFrameClassifyProductList.SkinFMXPanel1Resize(Sender: TObject);
begin

  Self.btnSortByDefault.Left:=(Self.SkinFMXPanel1.WidthInt-Self.btnSortByDefault.WidthInt*4 -2*3) div 2;
  Self.btnSortBySellCount.Left:=Self.btnSortByDefault.Left+Self.btnSortByDefault.WidthInt-2;
  Self.btnSortByGood.Left:=Self.btnSortBySellCount.Left+Self.btnSortBySellCount.WidthInt-2;
  Self.btnSortByPrice.Left:=Self.btnSortByGood.Left+Self.btnSortByGood.WidthInt-2;

end;

procedure TFrameClassifyProductList.tmrLoadingTimer(Sender: TObject);
var
  I: Integer;
  AListViewItem:TSkinListViewItem;
begin
  Self.tmrLoading.Enabled:=False;
  Self.plpTop.Properties.StopLoad;

  //  btnLoadNextData.Properties.IsPushed:=False;
//  btnLoadNextData.Caption:='显示下20条';


  Self.lbProductList.Properties.Items.BeginUpdate;
  try
    for I := 1 to 1 do
    begin
      AListViewItem:=Self.lbProductList.Properties.Items.Insert(Self.lbProductList.Properties.Items.Count-1);

      AListViewItem.Caption:=IntToStr(Self.lbProductList.Properties.Items.Count-1)+' '+'捷森 水果麦片 1kg';
      AListViewItem.Detail:='¥43.80';
      AListViewItem.Detail1:='德国';
      AListViewItem.Detail2:='1169条评论';
      AListViewItem.Icon.StaticImageIndex:=I Mod 2;

    end;
  finally
    Self.lbProductList.Properties.Items.EndUpdate;
  end;
end;

end.
