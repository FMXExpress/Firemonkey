unit ClassifyFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyLabel, uSkinFireMonkeyImage, uSkinFireMonkeyPanel,
  uUIFunction,
  SubClassifyFrame,
//  ClassifyProductListFrame,
  uSkinFireMonkeyControl, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uSkinFireMonkeyListView, uSkinFireMonkeyItemDesignerPanel, uDrawPicture,
  uSkinImageList;

type
  TFrameClassify = class(TFrame)
    SkinFMXListBox1: TSkinFMXListBox;
    imglistItemIcon: TSkinImageList;
    procedure SkinFMXListBox1ClickItem(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;

    { Public declarations }
  end;

var
  GlobalClassifyFrame:TFrameClassify;

implementation

{$R *.fmx}

uses
  MainForm,MainFrame;

{ TFrameClassify }

constructor TFrameClassify.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TFrameClassify.SkinFMXListBox1ClickItem(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  //显示产品列表界面
  ShowFrame(TFrame(GlobalSubClassifyFrame),TFrameSubClassify,frmMain,nil,nil,nil,Application);
  GlobalSubClassifyFrame.FrameHistroy:=CurrentFrameHistroy;

end;

end.
