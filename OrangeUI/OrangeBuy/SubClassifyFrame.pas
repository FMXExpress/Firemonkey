unit SubClassifyFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyLabel, uSkinFireMonkeyImage, uSkinFireMonkeyPanel,
  uUIFunction,
  ClassifyProductListFrame,
  uSkinFireMonkeyControl, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uSkinFireMonkeyListView, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyButton;

type
  TFrameSubClassify = class(TFrame)
    lbSubClassify: TSkinFMXListBox;
    pnlToolBar1: TSkinFMXPanel;
    SkinFMXPanel1: TSkinFMXPanel;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXListBox1: TSkinFMXListBox;
    procedure ItemDefaultResize(Sender: TObject);
    procedure SkinFMXButton1Click(Sender: TObject);
    procedure SkinFMXListBox1ClickItem(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalSubClassifyFrame:TFrameSubClassify;

implementation

{$R *.fmx}

uses
  MainForm,MainFrame;

{ TFrameSubClassify }

constructor TFrameSubClassify.Create(AOwner: TComponent);
begin
  inherited;
  Self.lbSubClassify.Properties.SelectedItem:=
    Self.lbSubClassify.Properties.Items[0];

end;

procedure TFrameSubClassify.ItemDefaultResize(Sender: TObject);
begin
//  //
//  Self.imgMenuIcon.Left:=(Self.ItemDefault.WidthInt-Self.imgMenuIcon.WidthInt) div 2;
end;

procedure TFrameSubClassify.SkinFMXButton1Click(Sender: TObject);
begin
  //显示主界面
  //返回
  HideFrame(Self);
  ReturnFrame(FrameHistroy);

end;

procedure TFrameSubClassify.SkinFMXListBox1ClickItem(Sender: TObject);
begin
  HideFrame(Self);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalClassifyProductListFrame),TFrameClassifyProductList,frmMain,nil,nil,nil,Application);
  GlobalClassifyProductListFrame.FrameHistroy:=CurrentFrameHistroy;

end;

end.
