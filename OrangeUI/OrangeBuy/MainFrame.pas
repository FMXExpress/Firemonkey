unit MainFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinPageControlType, uSkinFireMonkeyPageControl,
  HomeFrame,
  MyFrame,
  SearchFrame,
  ClassifyFrame,
  CartFrame,
  uSkinFireMonkeyNotifyNumberIcon, uSkinFireMonkeyControl,
  uSkinFireMonkeySwitchPageListPanel,uUIFunction;

type
  TFrameMain = class(TFrame)
    pcMain: TSkinFMXPageControl;
    tsSearch: TSkinFMXTabSheet;
    tsCart: TSkinFMXTabSheet;
    tsMy: TSkinFMXTabSheet;
    tsHome: TSkinFMXTabSheet;
    tsClassify: TSkinFMXTabSheet;
    nniCartCount: TSkinFMXNotifyNumberIcon;
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
  ShowFrame(TFrame(GlobalSearchFrame),TFrameSearch,tsSearch,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalClassifyFrame),TFrameClassify,tsClassify,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalCartFrame),TFrameCart,tsCart,nil,nil,nil,Application,False);
  ShowFrame(TFrame(GlobalMyFrame),TFrameMy,tsMy,nil,nil,nil,Application,False);



end;

procedure TFrameMain.pcMainResize(Sender: TObject);
begin
  Self.nniCartCount.Left:=(Self.pcMain.WidthInt div 5) * 3
      +(Self.pcMain.WidthInt div 5) div 2  + 3;

  Self.nniCartCount.Top:=(Self.pcMain.HeightInt
                        - Self.pcMain.Properties.TabHeaderHeight
                        + 2);

end;

end.
