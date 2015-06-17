unit StateFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinImageList, uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel, uDrawCanvas,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinFireMonkeyButton,
  uSkinListBoxType,uSkinItems,uUIFunction,
  uSkinFireMonkeyImage, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uDrawPicture, uSkinFireMonkeyVirtualList;

type
  TFrameState = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    imgToolBarDevide: TSkinFMXImage;
    imglistMenu: TSkinImageList;
    imglistState: TSkinImageList;
    lbMenus: TSkinFMXListBox;
    ItemMenu: TSkinFMXItemDesignerPanel;
    imgMenuIcon: TSkinFMXImage;
    lblMenuCaption: TSkinFMXLabel;
    imgExpandState: TSkinFMXImage;
    procedure lbMenusPrepareDrawItem(Sender: TObject;
      Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
      Item: TSkinItem; ItemRect: TRect);
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
  GlobalStateFrame:TFrameState;

implementation

{$R *.fmx}

procedure TFrameState.OnReturnFrame(FromFrame: TFrame);
begin
end;

constructor TFrameState.Create(AOwner: TComponent);
begin
  inherited;


  Self.lbMenus.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.lbMenus.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

end;

procedure TFrameState.lbMenusPrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
begin
//  Self.SkinFMXImage3.Properties.Picture.ImageIndex:=TSkinListBoxItem(Item).Icon.ImageIndex;
//  Self.SkinFMXLabel4.Caption:=TSkinListBoxItem(Item).Caption;
end;

end.
