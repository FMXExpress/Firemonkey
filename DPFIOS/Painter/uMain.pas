unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,

  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  iOSapi.CoreGraphics,
  iOSApi.UIKit,
  iOSApi.QuartzCore,
  iOSapi.Foundation,
  iOSapi.CoreText,
  iOSapi.CocoaTypes,
  Macapi.CoreFoundation,
  DPF.iOS.UIView, DPF.iOS.UISlider, DPF.iOS.UIButton;

type
  TFDrawingNative = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFSlider1: TDPFSlider;
    DPFButton1: TDPFButton;
    procedure FormShow( Sender: TObject );
    procedure DPFUIView1TouchesBegan( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint );
    procedure DPFUIView1TouchesMoved( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint );
    procedure DPFUIView1TouchesEnded( Sender: TObject; const TapCount: Integer; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint );
    procedure DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
    procedure DPFSlider1Changed( Sender: TObject; CurValue: Single );
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
    lineColor               : UIColor;
    DEFAULT_BACKGROUND_COLOR: UIColor;

    isEmpty: Boolean;

    lineWidth     : Single;
    previousPoint1: DPFNSPoint;
    previousPoint2: DPFNSPoint;
    currentPoint  : DPFNSPoint;

    path: CGMutablePathRef;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FDrawingNative: TFDrawingNative;

implementation

{$R *.fmx}

const
  kPointMinDistance        = 5;
  kPointMinDistanceSquared = kPointMinDistance * kPointMinDistance;

  // ---------------------------------------------------------------------------
function midPoint( p1: DPFNSPoint; p2: DPFNSPoint ): DPFNSPoint;
begin
  Result.x := ( p1.x + p2.x ) * 0.5;
  Result.y := ( p1.y + p2.y ) * 0.5;
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.DPFButton1Click( Sender: TObject );
begin
  path := CGPathCreateMutable( );
  DPFUIView1.GetUIView.setNeedsDisplay;
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.DPFSlider1Changed( Sender: TObject; CurValue: Single );
begin
  lineWidth := CurValue;
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
var
  context: CGContextRef;
begin
  // [self.backgroundColor set];
  // UIRectFill(rect);

  context := UIGraphicsGetCurrentContext( );

  CGContextAddPath( context, path );
  CGContextSetLineCap( context, kCGLineCapRound );
  CGContextSetLineWidth( context, self.lineWidth );
  CGContextSetStrokeColorWithColor( context, self.lineColor.CGColor );

  CGContextStrokePath( context );

  isEmpty := False;
end;

procedure TFDrawingNative.DPFUIView1TouchesBegan( Sender: TObject; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint );
begin
  previousPoint1 := PreviousLocation;
  previousPoint2 := PreviousLocation;
  currentPoint   := Location;
  DPFUIView1TouchesMoved( Sender, Location, PreviousLocation );
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.DPFUIView1TouchesMoved( Sender: TObject; const Location, PreviousLocation: DPFNSPoint );
var
  mid1, mid2, point: DPFNSPoint;
  dx, dy           : CGFloat;
  subpath          : CGMutablePathRef;
  bounds           : CGRect;
  drawBox          : CGRect;
begin

  point := Location;

  // check if the point is farther than min dist from previous
  dx := point.x - currentPoint.x;
  dy := point.y - currentPoint.y;

  if ( ( dx * dx + dy * dy ) < kPointMinDistanceSquared ) then
    Exit;

  previousPoint2 := previousPoint1;
  previousPoint1 := previousLocation;
  currentPoint   := location;

  mid1    := midPoint( previousPoint1, previousPoint2 );
  mid2    := midPoint( currentPoint, previousPoint1 );
  subpath := CGPathCreateMutable( );
  CGPathMoveToPoint( subpath, nil, mid1.x, mid1.y );
  CGPathAddQuadCurveToPoint( subpath, nil, previousPoint1.x, previousPoint1.y, mid2.x, mid2.y );
  bounds := CGPathGetBoundingBox( subpath );

  CGPathAddPath( path, nil, subpath );
  CGPathRelease( subpath );

  drawBox             := bounds;
  drawBox.origin.x    := drawBox.origin.x - self.lineWidth * 2.0;
  drawBox.origin.y    := drawBox.origin.y - self.lineWidth * 2.0;
  drawBox.size.width  := drawBox.size.width + self.lineWidth * 4.0;
  drawBox.size.height := drawBox.size.height + self.lineWidth * 4.0;

  DPFUIView1.GetUIView.setNeedsDisplayInRect( drawBox );
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.DPFUIView1TouchesEnded( Sender: TObject; const TapCount: Integer; const Location: DPFNSPoint; const PreviousLocation: DPFNSPoint );
begin
  { }
end;

// ---------------------------------------------------------------------------
procedure TFDrawingNative.FormShow( Sender: TObject );
begin
  lineWidth                := 5.0;
  lineColor                := TUIColor.Wrap( TUIColor.OCClass.blackColor );
  DEFAULT_BACKGROUND_COLOR := TUIColor.Wrap( TUIColor.OCClass.whiteColor );
  path                     := CGPathCreateMutable( );
end;

// -----------------------------------------------------------------------------
procedure TFDrawingNative.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

// -----------------------------------------------------------------------------
end.
