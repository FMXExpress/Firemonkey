unit MobileMsgFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyImage, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinFireMonkeyButton, uDrawPicture, uSkinImageList, uSkinFireMonkeyLabel,
  uSkinFireMonkeyItemDesignerPanel, uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox, uSkinFireMonkeyVirtualList;

type
  TFrameMobileMsg = class(TFrame)
    imgExpandState: TSkinImageList;
    imglstHead: TSkinImageList;
    lblProfile: TSkinFMXListBox;
    Item: TSkinFMXItemDesignerPanel;
    lblMenu: TSkinFMXLabel;
    SkinFMXImage3: TSkinFMXImage;
    imgMenu: TSkinFMXImage;
    pnlToolBar: TSkinFMXPanel;
    imgLine: TSkinFMXImage;
    btnStartTalking: TSkinFMXButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GlobalMobileMsgFrame:TFrameMobileMsg;

implementation

{$R *.fmx}

end.
