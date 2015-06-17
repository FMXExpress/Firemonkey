unit GuideFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, uSkinImageList, uSkinFireMonkeyControl, uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyImageListViewer, uSkinFireMonkeySwitchPageListPanel,
  uUIFunction,
  uSkinPageControlType, uSkinFireMonkeyPageControl, uSkinFireMonkeyButton;

type
  TFrameGuide = class(TFrame)
    imglistGuide: TSkinImageList;
    pcGuide: TSkinFMXPageControl;
    TSkinFMXTabSheet1: TSkinFMXTabSheet;
    TSkinFMXTabSheet2: TSkinFMXTabSheet;
    TSkinFMXTabSheet3: TSkinFMXTabSheet;
    TSkinFMXTabSheet4: TSkinFMXTabSheet;
    TSkinFMXTabSheet5: TSkinFMXTabSheet;
    TSkinFMXTabSheet6: TSkinFMXTabSheet;
    TSkinFMXTabSheet7: TSkinFMXTabSheet;
    TSkinFMXTabSheet8: TSkinFMXTabSheet;
    btnStart: TSkinFMXButton;
    procedure btnStartClick(Sender: TObject);
    procedure pcGuideResize(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalGuideFrame:TFrameGuide;

implementation

{$R *.fmx}
uses
  MainForm;


{ TFrameGuide }

constructor TFrameGuide.Create(AOwner: TComponent);
begin
  inherited;
  Self.pcGuide.Properties.TabHeaderHeight:=0;
  Self.pcGuide.Properties.ActivePageIndex:=0;

  Self.pcGuide.Properties.SwitchPageListControlGestureManager.ControlGestureManager.ScrollingToInitialAnimator.Speed:=3;
  Self.pcGuide.Properties.SwitchPageListControlGestureManager.ControlGestureManager.InertiaScrollAnimator.Speed:=3;

  Self.pcGuide.Properties.SwitchPageListControlGestureManager.CanGestureSwitch:=True;
  Self.pcGuide.Properties.SwitchPageListControlGestureManager.GestureSwitchLooped:=False;
  Self.pcGuide.Properties.SwitchPageListControlGestureManager.CanGestureSwitchDistance:=0.1;
end;

procedure TFrameGuide.btnStartClick(Sender: TObject);
begin
  HideFrame(Self);
//  frmMain.ShowLoginFrame;
end;

procedure TFrameGuide.pcGuideResize(Sender: TObject);
begin
  btnStart.Left:=(Self.pcGuide.WidthInt-btnStart.WidthInt) div 2;
end;

end.
