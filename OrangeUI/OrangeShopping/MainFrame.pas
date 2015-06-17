unit MainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinPageControlType, uSkinFireMonkeyPageControl,
  HomeFrame,
  MyFrame,
  ClassifyFrame,
  ShoppingCartFrame,
  uSkinFireMonkeyNotifyNumberIcon, uSkinFireMonkeyControl,
  uSkinFireMonkeySwitchPageListPanel,uUIFunction;

type
  TFrameMain = class(TFrame)
    pcMain: TSkinFMXPageControl;
    nniCartCount: TSkinFMXNotifyNumberIcon;
    tsClassify: TSkinFMXTabSheet;
    tsShoppingCart: TSkinFMXTabSheet;
    tsMy: TSkinFMXTabSheet;
    tsHome: TSkinFMXTabSheet;
    procedure pcMainResize(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalMainFrame:TFrameMain;

implementation

{$R *.fmx}

{ TFrameMain }

constructor TFrameMain.Create(AOwner: TComponent);
begin
  inherited;

  Self.pcMain.Properties.ActivePageIndex:=0;

  ShowFrame(TFrame(GlobalHomeFrame),TFrameHome,tsHome,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalClassifyFrame),TFrameClassify,tsClassify,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalShoppingCartFrame),TFrameShoppingCart,tsShoppingCart,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalMyFrame),TFrameMy,tsMy,nil,nil,nil,Application,False);



end;

procedure TFrameMain.pcMainResize(Sender: TObject);
begin
//  Self.nniCartCount.Left:=(Self.pcMain.WidthInt div 4) * 2
//      +(Self.pcMain.WidthInt div 4) div 2  + 3;
//
//  Self.nniCartCount.Top:=(Self.pcMain.HeightInt
//                        - Self.pcMain.Properties.TabHeaderHeight
//                        + 2);
//
end;

end.
