unit MyFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyImage,
  uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinImageList,
  uDrawPicture;

type
  TFrameMy = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    SkinFMXPanel1: TSkinFMXPanel;
    lbMenu: TSkinFMXListBox;
    ItemMenu: TSkinFMXItemDesignerPanel;
    lblMenuCaption: TSkinFMXLabel;
    imgExpandState: TSkinFMXImage;
    Header: TSkinFMXItemDesignerPanel;
    imgItemBackGround: TSkinFMXImage;
    imgHead: TSkinFMXImage;
    pnlItemBottom: TSkinFMXPanel;
    pnlWaitPay: TSkinFMXPanel;
    pnlWaitRecv: TSkinFMXPanel;
    lblWaitPayCount: TSkinFMXLabel;
    lblWaitPay: TSkinFMXLabel;
    lblWaitRecvCount: TSkinFMXLabel;
    lblWaitRecv: TSkinFMXLabel;
    imglistMenuIcon: TSkinImageList;
    imgMenuIcon: TSkinFMXImage;
    lblMenuDetail: TSkinFMXLabel;
    procedure lbMenuResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GlobalMyFrame:TFrameMy;


implementation

{$R *.fmx}

procedure TFrameMy.lbMenuResize(Sender: TObject);
begin
  //
  pnlWaitRecv.WidthInt:=Self.lbMenu.WidthInt div 2;
  pnlWaitPay.WidthInt:=Self.lbMenu.WidthInt div 2;
//  //
//  lblWaitRecv.WidthInt:=Self.lbMenu.WidthInt div 2;
//  lblWaitRecvCount.WidthInt:=Self.lbMenu.WidthInt div 2;
//  //
//  lblWaitPay.WidthInt:=Self.lbMenu.WidthInt div 2;
//  lblWaitPayCount.WidthInt:=Self.lbMenu.WidthInt div 2;
end;

end.
