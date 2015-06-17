unit HomeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uUIFunction, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinItems,
  uSkinListBoxType,
//  ProductInfoFrame,

  ThemeProductListFrame,
  ClassifyProductListFrame,

  uSkinFireMonkeyLabel, uSkinFireMonkeyImage, uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox, uSkinFireMonkeyItemDesignerPanel, uSkinImageList,
  uSkinFireMonkeyButton, uSkinButtonType, uSkinFireMonkeyImageListViewer,
  uDrawPicture, uSkinMaterial, uSkinFireMonkeyPullLoadPanel,
  uSkinPullLoadPanelType, uSkinFireMonkeyVirtualList;

type
  TFrameHome = class(TFrame)
    pnlToolBar1: TSkinFMXPanel;
    lbHome: TSkinFMXListBox;
    ItemHeader: TSkinFMXItemDesignerPanel;
    imgPlayer: TSkinFMXImageListViewer;
    imglistPlayer: TSkinImageList;
    bgIndicator: TSkinFMXButtonGroup;
    imglistLeft: TSkinImageList;
    imglistRight: TSkinImageList;
    ItemItem3: TSkinFMXItemDesignerPanel;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    imglistItem1: TSkinImageList;
    ItemItem1: TSkinFMXItemDesignerPanel;
    btnCircle: TSkinFMXButton;
    btnAddressBook: TSkinFMXButton;
    btnGroup: TSkinFMXButton;
    btnPublic: TSkinFMXButton;
    btnmItem1: TSkinButtonDefaultMaterial;
    SkinFMXPanel1: TSkinFMXPanel;
    SkinFMXImage1: TSkinFMXImage;
    ItemItem2: TSkinFMXItemDesignerPanel;
    SkinFMXButton4: TSkinFMXButton;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    btnmItem2: TSkinButtonDefaultMaterial;
    imglistItem2: TSkinImageList;
    pnlFooter1: TSkinFMXPanel;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXPanel5: TSkinFMXPanel;
    SkinFMXPanel6: TSkinFMXPanel;
    imgSearchBarLeft: TSkinFMXImage;
    imgSearchBarRightTop: TSkinFMXImage;
    imgSearchBarRightBottom: TSkinFMXImage;
    SkinFMXButton7: TSkinFMXButton;
    ItemItem4: TSkinFMXItemDesignerPanel;
    SkinFMXPanel3: TSkinFMXPanel;
    SkinFMXPanel4: TSkinFMXPanel;
    SkinFMXLabel2: TSkinFMXLabel;
    SkinFMXPanel9: TSkinFMXPanel;
    SkinFMXPanel7: TSkinFMXPanel;
    SkinFMXButton8: TSkinFMXButton;
    SkinFMXButton11: TSkinFMXButton;
    SkinFMXPanel8: TSkinFMXPanel;
    SkinFMXButton10: TSkinFMXButton;
    SkinFMXButton9: TSkinFMXButton;
    btnmItem4: TSkinButtonDefaultMaterial;
    imglistItem4: TSkinImageList;
    SkinFMXPanel10: TSkinFMXPanel;
    tmrLoading: TTimer;
    plpTop: TSkinFMXPullLoadPanel;
    lblTopLoading: TSkinFMXLabel;
    lblTopLoadInfo: TSkinFMXLabel;
    imgTopLoading: TSkinFMXImage;
    imgTopLoadHint: TSkinFMXImage;
    procedure tmrLoadingTimer(Sender: TObject);
    procedure imgPlayerStayClick(Sender: TObject);
    procedure imgSearchBarLeftStayClick(Sender: TObject);
    procedure imgSearchBarRightTopStayClick(Sender: TObject);
    procedure imgSearchBarRightBottomStayClick(Sender: TObject);
    procedure lbHomeClickItem(Sender: TObject);
    procedure lbHomeResize(Sender: TObject);
    procedure btnCircleClick(Sender: TObject);
    procedure plpTopExecuteLoad(Sender: TObject);
    procedure btnAddressBookClick(Sender: TObject);
    procedure btnGroupClick(Sender: TObject);
    procedure btnPublicClick(Sender: TObject);
    procedure SkinFMXButton4Click(Sender: TObject);
    procedure SkinFMXButton5Click(Sender: TObject);
    procedure SkinFMXButton6Click(Sender: TObject);
    procedure imgSearchBarLeftClick(Sender: TObject);
    procedure imgSearchBarRightTopClick(Sender: TObject);
    procedure imgSearchBarRightBottomClick(Sender: TObject);
    procedure imgPlayerClick(Sender: TObject);
    procedure SkinFMXButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalHomeFrame:TFrameHome;

implementation

{$R *.fmx}

uses
  MainForm,
  MainFrame;

{ TFrameHome }

procedure TFrameHome.btnAddressBookClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.btnCircleClick(Sender: TObject);
begin
  //查看分类
  GlobalMainFrame.pcMain.Properties.ActivePage:=GlobalMainFrame.tsClassify;
end;

procedure TFrameHome.btnGroupClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.btnPublicClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

constructor TFrameHome.Create(AOwner: TComponent);
begin
  inherited;


  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.lbHome.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;


  Self.imgPlayer.HorzScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.imgPlayer.HorzScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

//  Self.imgPlayer.HorzScrollBar.Properties.ControlGestureManager.IsCheckInertiaDirectionTime:=False;

//  tmrLoadingTimer(Self);

end;

procedure TFrameHome.imgPlayerClick(Sender: TObject);
begin

  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalThemeProductListFrame),TFrameThemeProductList,frmMain,nil,nil,nil,Application);
  GlobalThemeProductListFrame.FrameHistroy:=CurrentFrameHistroy;
end;

procedure TFrameHome.imgPlayerStayClick(Sender: TObject);
begin
//  HideFrame(GlobalMainFrame);

//  //显示产品列表界面
//  ShowFrame(TFrame(GlobalKindProductListFrame),TFrameKindProductList,frmMain,nil,nil,nil,Application);
//  GlobalKindProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.imgSearchBarLeftClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.imgSearchBarLeftStayClick(Sender: TObject);
begin

//  HideFrame(GlobalMainFrame);
//
//  //显示产品列表界面
//  ShowFrame(TFrame(GlobalKindProductListFrame),TFrameKindProductList,frmMain,nil,nil,nil,Application);
//  GlobalKindProductListFrame.FrameHistroy:=CurrentFrameHistroy;
end;

procedure TFrameHome.imgSearchBarRightBottomClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.imgSearchBarRightBottomStayClick(Sender: TObject);
begin
//  HideFrame(GlobalMainFrame);
//
//  //显示产品列表界面
//  ShowFrame(TFrame(GlobalKindProductListFrame),TFrameKindProductList,frmMain,nil,nil,nil,Application);
//  GlobalKindProductListFrame.FrameHistroy:=CurrentFrameHistroy;
//
end;

procedure TFrameHome.imgSearchBarRightTopClick(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.imgSearchBarRightTopStayClick(Sender: TObject);
begin
//  HideFrame(GlobalMainFrame);
//
//  //显示产品列表界面
//  ShowFrame(TFrame(GlobalKindProductListFrame),TFrameKindProductList,frmMain,nil,nil,nil,Application);
//  GlobalKindProductListFrame.FrameHistroy:=CurrentFrameHistroy;
//
end;

procedure TFrameHome.lbHomeClickItem(Sender: TObject);
begin
//  if TSkinItem(Sender).ItemType=sitDefault then
//  begin
//    HideFrame(GlobalMainFrame);
//
//    //显示产品信息界面
//    ShowFrame(TFrame(GlobalProductInfoFrame),TFrameProductInfo,frmMain,nil,nil,nil,Application);
//    GlobalProductInfoFrame.FrameHistroy:=CurrentFrameHistroy;
//  end;
end;

procedure TFrameHome.lbHomeResize(Sender: TObject);
begin
  Self.btnCircle.Width:=Self.lbHome.WidthInt div 4;
  Self.btnAddressBook.Width:=Self.lbHome.WidthInt div 4;
  Self.btnGroup.Width:=Self.lbHome.WidthInt div 4;
  Self.btnPublic.Width:=Self.lbHome.WidthInt div 4;


  Self.SkinFMXButton4.Width:=Self.lbHome.WidthInt div 3;
  Self.SkinFMXButton5.Width:=Self.lbHome.WidthInt div 3;
  Self.SkinFMXButton6.Width:=Self.lbHome.WidthInt div 3;

  Self.SkinFMXPanel7.Width:=Self.lbHome.WidthInt div 2;
  Self.SkinFMXPanel8.Width:=Self.lbHome.WidthInt div 2;

end;

procedure TFrameHome.plpTopExecuteLoad(Sender: TObject);
begin
  Self.tmrLoading.Enabled:=True;
end;

procedure TFrameHome.SkinFMXButton2Click(Sender: TObject);
begin
  //替换图片测试
  Self.imglistPlayer.PictureList.Items[0].FileName:='ecf23989835a6d221e2052efbfc235.jpg';


end;

procedure TFrameHome.SkinFMXButton4Click(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.SkinFMXButton5Click(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.SkinFMXButton6Click(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TFrameHome.tmrLoadingTimer(Sender: TObject);
//var
//  I: Integer;
//  AListBoxItem:TSkinListBoxItem;
begin
  Self.tmrLoading.Enabled:=False;

  Self.plpTop.Properties.StopLoad;

//  btnLoadNextData.Properties.IsPushed:=False;
//  btnLoadNextData.Caption:='显示下20条';
//
//
//  Self.lbHome.Properties.Items.BeginUpdate;
//  try
//    for I := 1 to 20 do
//    begin
//      AListBoxItem:=Self.lbHome.Properties.Items.Insert(Self.lbHome.Properties.Items.Count-1);
//
//      AListBoxItem.Caption:='测试数据'+IntToStr(I);
//      AListBoxItem.Detail:='￥563($90.00)';
//      AListBoxItem.Icon.StaticImageIndex:=I Mod 12;
//
//    end;
//  finally
//    Self.lbHome.Properties.Items.EndUpdate;
//  end;

end;

end.
