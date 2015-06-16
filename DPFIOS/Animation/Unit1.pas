// ------------------------------------------------------------------------
// Thanks to Diego Molina
//
// This demo mixed with FireMonkey
// ------------------------------------------------------------------------
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  FMX.Platform.iOS,

  DPF.iOS.BaseControl,
  DPF.iOS.UIButton, DPF.iOS.UIView, DPF.iOS.MKMapView, DPF.iOS.UILabel,
  DPF.iOS.UISegmentedControl, DPF.iOS.UIToolbar, DPF.iOS.UISwitch;

type
  TForm1 = class( TForm )
    DPFButton1: TDPFButton;
    MotherView: TDPFUIView;
    View2: TDPFUIView;
    View1: TDPFUIView;
    Mapa: TDPFMapView;
    DPFLabel1: TDPFLabel;
    DPFSegmentedControl1: TDPFSegmentedControl;
    DPFToolbar1: TDPFToolbar;
    SubView: TDPFUIView;
    DPFSwitch1: TDPFSwitch;
    DPFButton2: TDPFButton;
    DPFLabel2: TDPFLabel;
    DPFUIViewMain: TDPFUIView;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DPFButton1Click( Sender: TObject );
var
  Animation: DPF.iOS.BaseControl.TDPFViewAnimationTransition;
begin

  // TDPFViewAnimationTransition = ( vatNone = 0, vatFlipFromLeft = 1, vatFlipFromRight = 2, vatUp = 3, vatDown = 4 );
  case DPFSegmentedControl1.SelectedIndex of
    - 1, 0:
      Animation := vatNone;
    1:
      Animation := vatFlipFromLeft;
    2:
      Animation := vatFlipFromRight;
    3:
      Animation := vatUp;
    4:
      Animation := vatDown;
  end;

  // Animate MotherView
  MotherView.SetAnimationTransition( Animation, 1 );
  View2.Visible := not View2.Visible;
  View1.Visible := not View1.Visible;

  // Animate Button
  DPFButton1.SetAnimationTransition( Animation, 1 );
  if DPFButton1.Text = 'Click Me!' then
    DPFButton1.Text := 'Click me again!'
  else
    DPFButton1.Text := 'Click Me!';

end;

procedure TForm1.DPFButton2Click( Sender: TObject );
begin
  if SubView.Tag = 0 then
  begin
    SubView.AnimateFloat( 'Position.X', 140, 0.5 );
    SubView.Tag := 1;
  end
  else
  begin
    SubView.AnimateFloat( 'Position.X', 8, 0.5 );
    SubView.Tag := 0;
  end;
end;

procedure TForm1.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  if ISON then
    DPFButton2.AnimateFloat( 'Alpha', 1, 0.5 )
  else
    DPFButton2.AnimateFloat( 'Alpha', 0, 0.5 );
end;

procedure TForm1.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
