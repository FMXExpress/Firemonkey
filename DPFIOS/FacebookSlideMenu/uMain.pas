unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl, DPF.iOS.UIButton, DPF.iOS.UILabel, DPF.iOS.UIView,
  DPF.iOS.UIToolbar, DPF.iOS.UIImageView, DPF.iOS.UITableView,
  DPF.iOS.UIViewController;

type
  TFPanGestureRecognizer = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFImageView1: TDPFImageView;
    DPFUITableView1: TDPFUITableView;
    DPFUIViewController1: TDPFUIViewController;
    procedure FormShow( Sender: TObject );
    procedure DPFUIView2PanGestureRecognizer( Sender: TObject; CurPosition, Translation, Center, Velocity: DPFNSPoint; State: TDPFPanGestureRecognizeState; var MoveCenter: Boolean );
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFUIViewController1DidRotateFromInterfaceOrientation( Sender: TObject; Orientation: NativeUInt );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
    procedure initSlide( w, h: Single );
  end;

var
  FPanGestureRecognizer: TFPanGestureRecognizer;

implementation

{$R *.fmx}

var
  xWidth : Single = 0;
  xHeight: Single = 0;

procedure TFPanGestureRecognizer.initSlide( w, h: Single );
begin
  xWidth  := w;
  xHeight := h;
  if w = 0 then
    xWidth := width;

  if h = 0 then
    xHeight := Height;

  DPFUIView2.SetBounds( 0, 0, xWidth, xHeight );
  DPFUIView2.DoResize;
end;

procedure TFPanGestureRecognizer.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  if ( DPFUIView2.Position.x > xWidth / 2 ) then
  begin
    DPFUIView2.SetAnimationTransition( DPFUIView2.Position.X, 0, DPFUIView2.Position.y, DPFUIView2.Position.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 );
  end
  else
  begin
    DPFUIView2.SetAnimationTransition( DPFUIView2.Position.X, xWidth - 50, DPFUIView2.Position.y, DPFUIView2.Position.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 );
  end
end;

procedure TFPanGestureRecognizer.DPFUIView2PanGestureRecognizer( Sender: TObject; CurPosition, Translation, Center, Velocity: DPFNSPoint; State: TDPFPanGestureRecognizeState; var MoveCenter: Boolean );
var
  originalCenter: DPFNSPoint;
begin

  if State = grsBegan then
  begin
    originalCenter := center;

  end
  else if ( state = grsChanged ) then
  begin
    MoveCenter := true;
    // CGPoint translation = [recognizer translationInView:self.view];

    // recognizer.view.center = CGPointMake(originalCenter.x + translation.x, originalCenter.y);
  end
  else if ( state = grsEnded ) or ( state = grsCancelled ) or ( state = grsFailed ) then
  begin
    if ( Velocity.x > 0 ) and ( CurPosition.x > 50 ) then
      DPFUIView2.SetAnimationTransition( CurPosition.X, xWidth - 50, CurPosition.y, CurPosition.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 )
    else if ( Velocity.x > 0 ) and ( CurPosition.x < 50 ) then
      DPFUIView2.SetAnimationTransition( CurPosition.X, 0, CurPosition.y, CurPosition.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 )
    else if ( Velocity.x < 0 ) and ( CurPosition.x > 50 ) then
      DPFUIView2.SetAnimationTransition( CurPosition.X, 0, CurPosition.y, CurPosition.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 )
    else if ( Velocity.x < 0 ) and ( CurPosition.x > xWidth - 100 ) then
      DPFUIView2.SetAnimationTransition( CurPosition.X, xWidth - 50, CurPosition.y, CurPosition.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0.25 )
    else
      DPFUIView2.SetAnimationTransition( CurPosition.X, 0, CurPosition.y, CurPosition.y, DPFUIView2.Width, DPFUIView2.Width, DPFUIView2.Height, DPFUIView2.Height, 0 )

  end
  // MoveCenter := true;
end;

procedure TFPanGestureRecognizer.DPFUIViewController1DidRotateFromInterfaceOrientation( Sender: TObject; Orientation: NativeUInt );
begin
  initSlide( 0, 0 );
end;

procedure TFPanGestureRecognizer.FormShow( Sender: TObject );
begin
  initSlide( 0, 0 );
end;

procedure TFPanGestureRecognizer.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
