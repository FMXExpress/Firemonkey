unit ThemeProductListFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  uUIFunction,
  uSkinItems,
  Math,
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
  TFrameThemeProductList = class(TFrame)
    tmrLoading: TTimer;
    imglistProduct: TSkinImageList;
    pnlToolBar1: TSkinFMXPanel;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    btmColorButton: TSkinButtonDefaultMaterial;
    lbProductList: TSkinFMXListView;
    bdmBottomButton: TSkinButtonDefaultMaterial;
    plpTop: TSkinFMXPullLoadPanel;
    lblTopLoading: TSkinFMXLabel;
    imgTopLoading: TSkinFMXImage;
    imgTopLoadHint: TSkinFMXImage;
    iItemHeader: TSkinFMXItemDesignerPanel;
    SkinFMXImage1: TSkinFMXImage;
    ViewItemDefault: TSkinFMXItemDesignerPanel;
    pnlViewItemDefaultColor: TSkinFMXPanel;
    imgViewItemDefaultIcon: TSkinFMXImage;
    lblViewItemDefaultPrice: TSkinFMXLabel;
    lblViewItemDefaultDetail1: TSkinFMXLabel;
    lblViewItemDefaultCaption: TSkinFMXLabel;
    imglistTag: TSkinImageList;
    SkinFMXImage2: TSkinFMXImage;
    procedure btnReturnClick(Sender: TObject);
    procedure btnLoadNextDataClick(Sender: TObject);
    procedure tmrLoadingTimer(Sender: TObject);
    procedure lbProductListClickItem(Sender: TObject);
    procedure SkinFMXButton1Click(Sender: TObject);
    procedure plpTopExecuteLoad(Sender: TObject);
    procedure lbProductListResize(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;


var
  GlobalThemeProductListFrame:TFrameThemeProductList;

implementation

{$R *.fmx}

uses
  MainForm,MainFrame;

procedure TFrameThemeProductList.btnLoadNextDataClick(Sender: TObject);
begin
  //正在加载
  if not tmrLoading.Enabled then
  begin
    tmrLoading.Enabled:=True;
//    btnLoadNextData.Caption:='正在载入...';
  end;
end;

procedure TFrameThemeProductList.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

constructor TFrameThemeProductList.Create(AOwner: TComponent);
begin
  inherited;

  tmrLoadingTimer(Self);

end;

procedure TFrameThemeProductList.lbProductListClickItem(Sender: TObject);
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

procedure TFrameThemeProductList.lbProductListResize(Sender: TObject);
begin
//  if Self.lbProductList.Properties.Items.Count>0 then
//  begin
//    Self.lbProductList.Properties.Items[0].Width:=Self.lbProductList.WidthInt;
//  end;
  Self.lbProductList.Properties.ItemCountPerLine:=
    Ceil(Self.Width / Self.lbProductList.Properties.ItemWidth);
end;

procedure TFrameThemeProductList.plpTopExecuteLoad(Sender: TObject);
begin
  Self.tmrLoading.Enabled:=True;
end;

procedure TFrameThemeProductList.SkinFMXButton1Click(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameThemeProductList.tmrLoadingTimer(Sender: TObject);
var
  I: Integer;
  AListBoxItem:TSkinListViewItem;
begin
  Self.tmrLoading.Enabled:=False;
  Self.plpTop.Properties.StopLoad;

  //  btnLoadNextData.Properties.IsPushed:=False;
//  btnLoadNextData.Caption:='显示下20条';


  Self.lbProductList.Properties.Items.BeginUpdate;
  try
    for I := 1 to 10 do
    begin
      AListBoxItem:=Self.lbProductList.Properties.Items.Insert(Self.lbProductList.Properties.Items.Count-1);

      AListBoxItem.Caption:='天谱乐食Tender Plus澳州和牛牛腩条';
      AListBoxItem.Detail:='¥178';
      AListBoxItem.Detail1:='澳大利亚';
      AListBoxItem.Detail2:=IntToStr(I Mod 6);
      AListBoxItem.Icon.StaticImageIndex:=I Mod 2;

    end;
  finally
    Self.lbProductList.Properties.Items.EndUpdate;
  end;
end;

end.
