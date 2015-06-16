unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIView, iOSApi.CoreGraphics, iOSApi.UIKit, iOSApi.QuartzCore;

type
  TForm1034 = class(TForm)
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    procedure TMSFMXNativeUIView1DrawRect(Sender: TObject; ARect: TRectF);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1034: TForm1034;

implementation

uses
  Math;

{$R *.fmx}

procedure TForm1034.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIView1.View.setContentMode(UIViewContentModeRedraw);
end;

procedure TForm1034.TMSFMXNativeUIView1DrawRect(Sender: TObject; ARect: TRectF);
var
  context: CGContextRef;
  w, h: Double;
  tr: CGAffineTransform;
begin
  context := UIGraphicsGetCurrentContext();
  CGContextMoveToPoint(context, 100, 100);
  CGContextAddLineToPoint(context, 150, 150);
  CGContextAddLineToPoint(context, 100, 200);
  CGContextAddLineToPoint(context, 50, 150);
  CGContextAddLineToPoint(context, 100, 100);
  CGContextSetFillColorWithColor(context, TUIColor.Wrap(TUIColor.OCClass.redColor).CGColor);
  CGContextFillPath(context);

  CGContextSetLineWidth(context, 4.0);
  CGContextSetStrokeColorWithColor(context, TUIColor.Wrap(TUIColor.OCClass.greenColor).CGColor);
  CGContextMoveToPoint(context, 200, 100);
  CGContextAddArcToPoint(context, 200,200, 300,200, 100);
  CGContextStrokePath(context);

  h := ARect.Height;

  CGContextTranslateCTM(context, 0, h);
  CGContextScaleCTM(context, 1, -1);

  CGContextSelectFont(context,
                  UTF8ToString(TUIFont.Wrap(TUIFont.OCClass.boldSystemFontOfSize(h/10)).fontName.UTF8String),
                   h/10,
                   kCGEncodingMacRoman);
  CGContextSetCharacterSpacing(context, 10);
  CGContextSetTextDrawingMode(context, kCGTextFillStroke);

  CGContextSetRGBFillColor(context, 0, 1, 0, 0.5);
  CGContextSetRGBStrokeColor(context, 0, 0, 1, 1);
  tr := CGAffineTransformMakeRotation(DegToRad(25));
  CGContextSetTextMatrix(context, tr);
  CGContextShowTextAtPoint(context, 30, 30, 'Quartz 2D', 9);
end;

end.
