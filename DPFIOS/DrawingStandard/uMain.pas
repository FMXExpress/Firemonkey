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
  DPF.iOS.UIView;

type
  TFDrawingNative = class( TForm )
    DPFUIView1: TDPFUIView;
    procedure DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FDrawingNative: TFDrawingNative;

implementation

{$R *.fmx}

procedure TFDrawingNative.DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
var
  R: DPFNSRect;
begin

  // Draw Line 1
  DPFUIView1.DrawLine( MakeDPFNSPoint( 0, 0 ), MakeDPFNSPoint( Width, height ), 15, TAlphaColors.Red );

  // Draw Line 1
  DPFUIView1.DrawLine( MakeDPFNSPoint( Width, 0 ), MakeDPFNSPoint( 0, height ), 15, TAlphaColors.Blue );

  // Draw Rectangle
  R.origin.x    := 0;
  R.origin.y    := 0;
  R.size.width  := width;
  R.size.height := Height;
  DPFUIView1.DrawRect( R, 15, TAlphaColors.Green, TAlphaColors.Null );

  // Draw Circle
  DPFUIView1.DrawArc( Width / 2, Height / 2, 100, 0, 360, true, 15, TAlphaColors.Brown );

  // Draw Ellipse
  R.origin.x    := 100;
  R.origin.y    := 100;
  R.size.width  := 200;
  R.size.height := 100;
  DPFUIView1.DrawEllipse( R, 15, TAlphaColors.Forestgreen, TAlphaColors.Null );

  DPFUIView1.DrawText( 'D.P.F iOS Components', 10, 280, 1, TAlphaColors.Midnightblue, TAlphaColors.Red, 0, kCGTextFillStrokeClip, 1, 'Helvetica', 24 );
end;

procedure TFDrawingNative.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
