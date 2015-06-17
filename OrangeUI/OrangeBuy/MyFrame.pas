unit MyFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyImage,
  uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinImageList,
  uDrawPicture, uSkinMaterial, uSkinButtonType, uSkinFireMonkeyButton,
  uUIFunction,
  LoginFrame,
  uSkinImageType;

type
  TFrameMy = class(TFrame)
    lbMenu: TSkinFMXListBox;
    ItemMenu: TSkinFMXItemDesignerPanel;
    lblMenuCaption: TSkinFMXLabel;
    imgExpandState: TSkinFMXImage;
    Header: TSkinFMXItemDesignerPanel;
    imgItemBackGround: TSkinFMXImage;
    imgHead: TSkinFMXImage;
    pnlItemBottom: TSkinFMXPanel;
    imglistItem1: TSkinImageList;
    btnmItem1: TSkinButtonDefaultMaterial;
    btnCircle: TSkinFMXButton;
    btnAddressBook: TSkinFMXButton;
    btnGroup: TSkinFMXButton;
    btnPublic: TSkinFMXButton;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXImage2: TSkinFMXImage;
    SkinFMXImage3: TSkinFMXImage;
    idmDevide: TSkinImageDefaultMaterial;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXButton1: TSkinFMXButton;
    bdmLoginButton: TSkinButtonDefaultMaterial;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    procedure lbMenuResize(Sender: TObject);
    procedure SkinFMXButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GlobalMyFrame:TFrameMy;


implementation

{$R *.fmx}

uses
  MainForm,
  MainFrame;


procedure TFrameMy.lbMenuResize(Sender: TObject);
begin
  //
//  pnlWaitRecv.WidthInt:=Self.lbMenu.WidthInt div 2;
//  pnlWaitPay.WidthInt:=Self.lbMenu.WidthInt div 2;
//  //
//  lblWaitRecv.WidthInt:=Self.lbMenu.WidthInt div 2;
//  lblWaitRecvCount.WidthInt:=Self.lbMenu.WidthInt div 2;
//  //
//  lblWaitPay.WidthInt:=Self.lbMenu.WidthInt div 2;
//  lblWaitPayCount.WidthInt:=Self.lbMenu.WidthInt div 2;
end;

procedure TFrameMy.SkinFMXButton2Click(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  ShowFrame(TFrame(GlobalLoginFrame),TFrameLogin,frmMain,nil,nil,nil,Application,True);
  GlobalLoginFrame.FrameHistroy:=CurrentFrameHistroy;
end;

end.
