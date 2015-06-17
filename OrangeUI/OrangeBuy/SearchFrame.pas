unit SearchFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyButton, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uSkinFireMonkeyItemDesignerPanel, uDrawPicture, uSkinImageList,
  uSkinFireMonkeyImage, uSkinFireMonkeyLabel;

type
  TFrameSearch = class(TFrame)
    pnlToolBar1: TSkinFMXPanel;
    SkinFMXButton3: TSkinFMXButton;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXListBox1: TSkinFMXListBox;
    ItemSearchBar: TSkinFMXItemDesignerPanel;
    ItemDefault: TSkinFMXItemDesignerPanel;
    imgItemIcon: TSkinFMXImage;
    imglistItemIcon: TSkinImageList;
    lblItemSearchBarCaption: TSkinFMXLabel;
    lblItemDefaultCaption: TSkinFMXLabel;
    imgItemDefaultAdd: TSkinFMXImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  GlobalSearchFrame:TFrameSearch;

implementation

{$R *.fmx}




end.
