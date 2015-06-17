unit MobileFindFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uUIFunction,
  uSkinFireMonkeyImage, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinFireMonkeyLabel, uDrawPicture, uSkinImageList,
  uSkinFireMonkeyItemDesignerPanel, uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox, uSkinFireMonkeyButton;

type
  TFrameMobileFind = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    imgLine: TSkinFMXImage;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXImage2: TSkinFMXImage;
    SkinFMXLabel1: TSkinFMXLabel;
    lblProfile: TSkinFMXListBox;
    Item: TSkinFMXItemDesignerPanel;
    imgMenu: TSkinFMXImage;
    imglstHead: TSkinImageList;
    SkinFMXPanel1: TSkinFMXPanel;
    lblMenu: TSkinFMXLabel;
    lblMenuDetail: TSkinFMXLabel;
    ItemHeader: TSkinFMXItemDesignerPanel;
    SkinFMXImage3: TSkinFMXImage;
    procedure SkinFMXImage1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GlobalMobileFindFrame:TFrameMobileFind;

implementation

{$R *.fmx}

procedure TFrameMobileFind.SkinFMXImage1Click(Sender: TObject);
begin
  //´ò¿ªËÑË÷Ò³Ãæ

end;

end.
