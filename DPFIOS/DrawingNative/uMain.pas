unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,

  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  Macapi.ObjectiveC,
  iOSapi.CoreGraphics,
  iOSApi.UIKit,
  iOSApi.QuartzCore,
  iOSapi.Foundation,
  iOSapi.CoreText,
  iOSapi.CocoaTypes,
  Macapi.CoreFoundation,
  DPF.iOS.UIView;

type
  TFDrawingNative = class( TForm )
    DPFUIView1: TDPFUIView;
    procedure DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
  private
    { Private declarations }
    procedure DrawGradient;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FDrawingNative: TFDrawingNative;

implementation

{$R *.fmx}

procedure TFDrawingNative.DrawGradient;
var
  imageBounds: CGRect;
  alignStroke: CGFloat;
  resolution : CGFloat;
  path       : CGMutablePathRef;
  drawRect   : CGRect;
  gradient   : CGGradientRef;
  colors     : NSMutableArray;
  color      : UIColor;
  space      : CGColorSpaceRef;
  context    : CGContextRef;

  point, point2: CGPoint;
  locations    : array [0 .. 4] of CGFloat;
begin
  imageBounds := CGRectMake( 0.0, 0.0, DPFUIView1.width, DPFUIView1.height );
  space       := CGColorSpaceCreateDeviceRGB( );
  context     := UIGraphicsGetCurrentContext( );

  resolution := 0.5 * ( DPFUIView1.width / imageBounds.size.width + DPFUIView1.height / imageBounds.size.height );

  alignStroke          := 0.0;
  path                 := CGPathCreateMutable( );
  drawRect             := CGRectMake( 0.0, 0.0, DPFUIView1.width, DPFUIView1.height );
  drawRect.origin.x    := ( round( resolution * drawRect.origin.x + alignStroke ) - alignStroke ) / resolution;
  drawRect.origin.y    := ( round( resolution * drawRect.origin.y + alignStroke ) - alignStroke ) / resolution;
  drawRect.size.width  := round( resolution * drawRect.size.width ) / resolution;
  drawRect.size.height := round( resolution * drawRect.size.height ) / resolution;
  CGPathAddRect( path, nil, drawRect );
  colors := TNSMutableArray.Wrap( TNSMutableArray.OCClass.arrayWithCapacity( 5 ) );
  color  := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( 0.351, 0.444, 0.573, 1.0 ) );
  colors.addObject( color.CGColor );
  locations[0] := 0.0;
  color        := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( 0.62, 0.676, 0.754, 1.0 ) );
  colors.addObject( color.CGColor );
  locations[1] := 1.0;
  color        := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( 0.563, 0.627, 0.713, 1.0 ) );
  colors.addObject( color.CGColor );
  locations[2] := 0.743;
  color        := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( 0.479, 0.553, 0.66, 1.0 ) );
  colors.addObject( color.CGColor );
  locations[3] := 0.498;
  color        := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( 0.43, 0.51, 0.63, 1.0 ) );
  colors.addObject( color.CGColor );
  locations[4] := 0.465;
  gradient     := CGGradientCreateWithColors( space, ( colors as ILocalObject ).GetObjectID, @locations[0] );
  CGContextAddPath( context, path );
  CGContextSaveGState( context );
  CGContextEOClip( context );
  point  := CGPointMake( 100.0, DPFUIView1.height );
  point2 := CGPointMake( 100.0, 0.0 );
  CGContextDrawLinearGradient( context, gradient, point, point2, ( kCGGradientDrawsBeforeStartLocation or kCGGradientDrawsAfterEndLocation ) );
  CGContextRestoreGState( context );
  CGGradientRelease( gradient );
  CGPathRelease( path );
  CGColorSpaceRelease( space );
end;

procedure TFDrawingNative.DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
const
  components: array [0 .. 3] of CGFloat = ( 0.0, 0.0, 1.0, 1.0 );

  locs: array [0 .. 2] of CGFloat    = ( 0.0, 0.5, 1.0 );
  colors: array [0 .. 11] of CGFloat = ( 0.3, 0.3, 0.3, 0.8, // starting color, transparent gray
    0.0, 0.0, 0.0, 1.0, // intermediate color, black
    0.3, 0.3, 0.3, 0.8// ending color, transparent gray
    );

var
  currentContext, context: CGContextRef;
  colorspace             : CGColorSpaceRef;
  color                  : CGColorRef;
  rectangle              : CGRect;
  colStep                : CGFloat;
  startColor             : UIColor;
  startColorComponents   : ^CGFloat;

  con : CGContextRef;
  sp  : CGColorSpaceRef;
  grad: CGGradientRef;

begin

  DrawGradient;

  context := UIGraphicsGetCurrentContext( );

  // --------------------------------------------------------------
  // Draw Line
  CGContextSetLineWidth( context, 5.0 );
  colorspace := CGColorSpaceCreateDeviceRGB( );
  color      := CGColorCreate( colorspace, @components[0] );
  CGContextSetStrokeColorWithColor( context, color );
  CGContextMoveToPoint( context, width / 2, 0 );
  CGContextAddLineToPoint( context, width / 2, Height );
  CGContextStrokePath( context );

  // --------------------------------------------------------------
  // Draw Arc
  CGContextSetLineWidth( context, 3.0 );
  CGContextSetStrokeColorWithColor( context, TUIColor.Wrap( TUIColor.OCClass.magentaColor ).CGColor );
  rectangle := CGRectMake( 60, 170, 200, 80 );
  CGContextAddEllipseInRect( context, rectangle );
  CGContextStrokePath( context );

  // --------------------------------------------------------------
  // Drawing a Cubic Bezier Curve
  CGContextSetLineWidth( context, 2.0 );
  CGContextSetStrokeColorWithColor( context, TUIColor.Wrap( TUIColor.OCClass.redColor ).CGColor );
  CGContextMoveToPoint( context, 10, 10 );
  CGContextAddCurveToPoint( context, 0, 50, 300, 250, 300, 400 );
  CGContextStrokePath( context );

  // --------------------------------------------------------------
  // Drawing a Quadratic Bezier Curve
  CGContextSetLineWidth( context, 2.0 );
  CGContextSetStrokeColorWithColor( context, TUIColor.Wrap( TUIColor.OCClass.blueColor ).CGColor );
  CGContextMoveToPoint( context, 10, 200 );
  CGContextAddQuadCurveToPoint( context, 150, 10, 300, 200 );
  CGContextStrokePath( context );

  // --------------------------------------------------------------
  // Drawing Gradiant
  con := UIGraphicsGetCurrentContext( );
  CGContextSaveGState( con );

  // punch triangular hole in context clipping region
  CGContextMoveToPoint( con, 90, 100 );
  CGContextAddLineToPoint( con, 100, 90 );
  CGContextAddLineToPoint( con, 110, 100 );
  CGContextClosePath( con );
  CGContextAddRect( con, CGContextGetClipBoundingBox( con ) );
  CGContextEOClip( con );

  // draw the vertical line, add its shape to the clipping region
  CGContextMoveToPoint( con, 100, 100 );
  CGContextAddLineToPoint( con, 100, 19 );
  CGContextSetLineWidth( con, 20 );
  CGContextReplacePathWithStrokedPath( con );
  CGContextClip( con );

  // draw the gradient
  sp   := CGColorSpaceCreateDeviceGray( );
  grad := CGGradientCreateWithColorComponents( sp, @colors[0], @locs[0], 3 );
  CGContextDrawLinearGradient( con, grad, CGPointMake( 89, 0 ), CGPointMake( 111, 0 ), 0 );
  CGColorSpaceRelease( sp );
  CGGradientRelease( grad );

  CGContextRestoreGState( con ); // done clipping

  // draw the red triangle, the point of the arrow
  CGContextSetFillColorWithColor( con, TUIColor.Wrap( TUIColor.OCClass.redColor ).CGColor );
  CGContextMoveToPoint( con, 80, 25 );
  CGContextAddLineToPoint( con, 100, 0 );
  CGContextAddLineToPoint( con, 120, 25 );
  CGContextFillPath( con );

  // --------------------------------------------------------------
  // Draw & Release
  CGContextStrokePath( context );
  CGColorSpaceRelease( colorspace );
  CGColorRelease( color );
end;

procedure TFDrawingNative.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
