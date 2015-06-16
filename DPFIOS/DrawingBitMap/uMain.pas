unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
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
  FMX.TextLayout,
  FMX.Graphics,
  DPF.iOS.UIButton, DPF.iOS.UIView;

type
  TFDrawingBitMap = class( TForm )
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
  FDrawingBitMap: TFDrawingBitMap;

implementation

{$R *.fmx}

procedure TFDrawingBitMap.DPFUIView1DrawRect( Sender: TObject; Rect: DPFNSRect );
var
  BMP      : TBitMap;
  Img      : UIImage;
  C        : TCanvas;
  SB       : TStrokeBrush;
  imageRect: CGRect;
begin
  BMP := TBitMap.Create( Round( Rect.size.width ), Round( Rect.size.height ) );
  BMP.Canvas.BeginScene( );
  BMP.Canvas.Clear( TAlphaColors.Null );
  BMP.Canvas.StrokeThickness := 10;

  BMP.Canvas.Stroke.Kind  := TBrushKind.bkSolid;
  BMP.Canvas.Stroke.Color := TAlphaColors.Green;
  BMP.Canvas.DrawLine( TPointF.Create( 0, 0 ), TPointF.Create( Width, Height ), 1 );
  BMP.Canvas.Stroke.Color := TAlphaColors.Blue;
  BMP.Canvas.DrawLine( TPointF.Create( Width, 0 ), TPointF.Create( 0, Height ), 1 );

  BMP.Canvas.StrokeThickness := 2;
  BMP.Canvas.Stroke.Color    := TAlphaColors.Green;
  BMP.Canvas.Fill.Color      := TAlphaColors.Red;
  BMP.Canvas.Font.Size       := 32;
  BMP.Canvas.FillText( TRectF.Create( TPointF.Create( 1, 100 ), Width, 200 ), 'D.P.F iOS Native Components', true, 1, [], TTextAlign.taCenter, TTextAlign.taCenter );

  BMP.Canvas.StrokeThickness := 5;
  BMP.Canvas.Stroke.Color    := TAlphaColors.Brown;
  BMP.Canvas.Fill.Color      := TAlphaColors.Null;
  BMP.Canvas.DrawRect( TRectF.Create( TPointF.Create( 40, 40 ), TPointF.Create( width - 40, height - 40 ) ), 5, 5, AllCorners, 1 );

  BMP.Canvas.EndScene;

  Img       := BitmapToUIImage( Bmp );
  imageRect := CGRectMake( 0, 0, Rect.size.width, Rect.size.height );
  Img.drawInRect( imageRect );
  BMP.Free;
end;

procedure TFDrawingBitMap.PaintRects( const UpdateRects: array of TRectF );
begin

  { }

end;

end.
