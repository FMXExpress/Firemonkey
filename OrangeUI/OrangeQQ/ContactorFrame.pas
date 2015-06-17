unit ContactorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Math,uUIFunction,
  uSkinTreeViewType,
  uSkinImageList, uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinFireMonkeyImage,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyButton,
  uSkinFireMonkeyTreeView, uSkinFireMonkeyNotifyNumberIcon, uSkinMaterial,
  uSkinItemDesignerPanelType, uSkinButtonType, uDrawPicture,
  uSkinFireMonkeyVirtualList;

type
  TFrameContactor = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    imgToolBarDevide: TSkinFMXImage;
    pnlClient: TSkinFMXPanel;
    tvContactor: TSkinFMXTreeView;
    ItemSearchBar: TSkinFMXItemDesignerPanel;
    imgSearchBackground: TSkinFMXImage;
    imgSearchIcon: TSkinFMXImage;
    lblSearchHint: TSkinFMXLabel;
    ItemContactor: TSkinFMXItemDesignerPanel;
    imgMessageHead: TSkinFMXImage;
    lblMessageNickName: TSkinFMXLabel;
    lblMessageDetail: TSkinFMXLabel;
    imglistHead: TSkinImageList;
    ItemGroup: TSkinFMXItemDesignerPanel;
    imgGroupExpanded: TSkinFMXImage;
    lblGroupName: TSkinFMXLabel;
    lblGroupFriendCount: TSkinFMXLabel;
    ItemHeader: TSkinFMXItemDesignerPanel;
    lblHeaderCaption: TSkinFMXLabel;
    ItemFunctionPanel: TSkinFMXItemDesignerPanel;
    btnmFunction: TSkinButtonDefaultMaterial;
    btnCircle: TSkinFMXButton;
    imglistFunction: TSkinImageList;
    btnAddressBook: TSkinFMXButton;
    btnGroup: TSkinFMXButton;
    btnPublic: TSkinFMXButton;
    imglistExpanded: TSkinImageList;
    imgMessageNetworkState: TSkinFMXImage;
    imglistNetworkState: TSkinImageList;
    btnAdd: TSkinFMXButton;
    procedure tvContactorResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
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
  GlobalContactorFrame:TFrameContactor;

implementation

{$R *.fmx}

procedure TFrameContactor.btnAddClick(Sender: TObject);
var
  AChildTreeViewItem:TSkinTreeViewItem;
  AParentTreeViewItem:TSkinTreeViewItem;
  I: Integer;
begin
  AParentTreeViewItem:=TSkinTreeViewItem(Self.tvContactor.Properties.Items.FindItemByCaption('我的好友'));

  for I := 0 to 10 do
  begin
    AChildTreeViewItem:=AParentTreeViewItem.Childs.Add;
    AChildTreeViewItem.Caption:='测试好友'+IntToStr(I);
    AChildTreeViewItem.Icon.ImageIndex:=4;
    AChildTreeViewItem.Detail:='[离线]您好，我找小白兔去了，有事稍后联系！';
    AChildTreeViewItem.Detail1:='8';
  end;
end;

constructor TFrameContactor.Create(AOwner: TComponent);
begin
  inherited;

  btnAddClick(Self);

  Self.tvContactor.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.tvContactor.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

end;

procedure TFrameContactor.OnReturnFrame(FromFrame: TFrame);
begin

end;

procedure TFrameContactor.tvContactorResize(Sender: TObject);
begin
  Self.btnCircle.Width:=Self.tvContactor.WidthInt div 4;
  Self.btnAddressBook.Width:=Self.tvContactor.WidthInt div 4;
  Self.btnGroup.Width:=Self.tvContactor.WidthInt div 4;
  Self.btnPublic.Width:=Self.tvContactor.WidthInt div 4;
end;

end.
