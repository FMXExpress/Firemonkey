unit SettingFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinImageList, uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel,
  uUIFunction,uDrawCanvas,uSkinItems,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinFireMonkeyButton,
  uSkinFireMonkeyImage, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uDrawPicture, uSkinFireMonkeyVirtualList;

type
  TFrameSetting = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    imgToolBarDevide: TSkinFMXImage;
    lbMenu: TSkinFMXListBox;
    ItemMenu: TSkinFMXItemDesignerPanel;
    Header: TSkinFMXItemDesignerPanel;
    lblMenuCaption: TSkinFMXLabel;
    imgHead: TSkinFMXImage;
    lblNickName: TSkinFMXLabel;
    imgExpandState: TSkinFMXImage;
    imglistState: TSkinImageList;
    imgExpandState1: TSkinFMXImage;
    SkinFMXImage5: TSkinFMXImage;
    SkinFMXImage6: TSkinFMXImage;
    SkinFMXImage7: TSkinFMXImage;
    SkinFMXImage8: TSkinFMXImage;
    SkinFMXImage9: TSkinFMXImage;
    SkinFMXImage10: TSkinFMXImage;
    SkinFMXImage11: TSkinFMXImage;
    SkinFMXImage12: TSkinFMXImage;
    SkinFMXImage13: TSkinFMXImage;
    imglistLevel: TSkinImageList;
    procedure lbMenuPrepareDrawItem(Sender: TObject;
      Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
      Item: TSkinItem; ItemRect: TRect);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
  public
    { Public declarations }
  end;



var
  GlobalSettingFrame:TFrameSetting;

implementation

{$R *.fmx}

{ TFrameSetting }

procedure TFrameSetting.OnReturnFrame(FromFrame: TFrame);
begin

end;

procedure TFrameSetting.lbMenuPrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
begin
//  Self.SkinFMXLabel4.Caption:=Item.Caption;
end;

end.
