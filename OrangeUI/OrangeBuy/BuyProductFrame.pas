unit BuyProductFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uUIFunction,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyButton,
  uSkinFireMonkeyLabel, uSkinFireMonkeyImage, FMX.Controls.Presentation,
  FMX.Edit, uSkinFireMonkeyEdit;

type
  TFrameBuyProduct = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXPanel1: TSkinFMXPanel;
    SkinFMXButton7: TSkinFMXButton;
    ItemDefault: TSkinFMXPanel;
    imgDefaultIcon: TSkinFMXImage;
    lblDefaultCaption: TSkinFMXLabel;
    lblDefaultPrice: TSkinFMXLabel;
    SkinFMXLabel3: TSkinFMXLabel;
    imgBuy: TSkinFMXImage;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXPanel3: TSkinFMXPanel;
    SkinFMXLabel4: TSkinFMXLabel;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXEdit1: TSkinFMXEdit;
    SkinFMXPanel6: TSkinFMXPanel;
    SkinFMXPanel8: TSkinFMXPanel;
    SkinFMXPanel7: TSkinFMXPanel;
    procedure SkinFMXButton7Click(Sender: TObject);
    procedure imgBuyResize(Sender: TObject);
    procedure SkinFMXButton5Click(Sender: TObject);
    procedure SkinFMXButton2Click(Sender: TObject);
    procedure SkinFMXButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    { Public declarations }
  end;


var
  GlobalBuyProductFrame:TFrameBuyProduct;

implementation

{$R *.fmx}

procedure TFrameBuyProduct.imgBuyResize(Sender: TObject);
begin
  SkinFMXButton5.Left:=(Self.imgBuy.WidthInt-SkinFMXButton5.WidthInt) div 2;
end;

procedure TFrameBuyProduct.SkinFMXButton1Click(Sender: TObject);
begin
  if StrToInt(Self.SkinFMXEdit1.Text)>1 then
  begin
    Self.SkinFMXEdit1.Text:=IntToStr(StrToInt(Self.SkinFMXEdit1.Text)-1);
  end;

end;

procedure TFrameBuyProduct.SkinFMXButton2Click(Sender: TObject);
begin
  Self.SkinFMXEdit1.Text:=IntToStr(StrToInt(Self.SkinFMXEdit1.Text)+1);
end;

procedure TFrameBuyProduct.SkinFMXButton5Click(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

procedure TFrameBuyProduct.SkinFMXButton7Click(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

end.
