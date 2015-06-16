unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UITabBarController,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.UISlider,
  DPF.iOS.UIToolbar,
  DPF.iOS.UISegmentedControl,
  DPF.iOS.MPVolume,
  DPF.iOS.UISwitch,
  DPF.iOS.MKMapView,
  DPF.iOS.UIStepper,
  FMX.StdCtrls,
  DPF.iOS.UIViewController;

type
  TFTabBar = class( TForm )
    DPFTabBarController1: TDPFTabBarController;
    DPFTabBarItem1: TDPFTabBarItem;
    DPFTabBarItem2: TDPFTabBarItem;
    DPFSlider1: TDPFSlider;
    DPFToolbar1: TDPFToolbar;
    DPFSwitch1: TDPFSwitch;
    DPFMPVolume1: TDPFMPVolume;
    DPFSegmentedControl1: TDPFSegmentedControl;
    DPFTabBarItem3: TDPFTabBarItem;
    DPFMapView1: TDPFMapView;
    DPFStepper1: TDPFStepper;
    DPFTabBarItem4: TDPFTabBarItem;
    procedure DPFStepper1Changed( Sender: TObject; CurValue: Double );
    procedure DPFTabBarController1ChangingPage( Sender: TObject; FromIndedx, ToIndex: Integer; var CanChange: Boolean );
    procedure DPFTabBarController1SelectPage( Sender: TObject; SelectedIndex: Integer );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTabBar: TFTabBar;

implementation

{$R *.fmx}

procedure TFTabBar.DPFStepper1Changed( Sender: TObject; CurValue: Double );
begin
  DPFTabBarItem2.BadgeValue := IntToStr( Round( DPFStepper1.Value ) );
end;

procedure TFTabBar.DPFTabBarController1ChangingPage( Sender: TObject; FromIndedx, ToIndex: Integer; var CanChange: Boolean );
begin
  { }
end;

procedure TFTabBar.DPFTabBarController1SelectPage( Sender: TObject; SelectedIndex: Integer );
begin
  { }
  //ShowMessage( DPFTabBarController1.ActivePageIndex.ToString + ' , ' + SelectedIndex.ToString );
end;

procedure TFTabBar.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
