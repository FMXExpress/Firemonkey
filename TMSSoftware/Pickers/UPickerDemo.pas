unit UPickerDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBitmapSelector, FMX.TMSCustomSelector, FMX.TMSColorSelector,
  FMX.TMSBitmapPicker, FMX.TMSBaseControl, FMX.TMSCustomPicker,
  FMX.TMSColorPicker, FMX.Objects, System.UIConsts, FMX.TMSFontSizePicker,
  FMX.TMSFontNamePicker, FMX.Layouts, FMX.Memo;

type
  TForm5 = class(TForm)
    GroupBox1: TGroupBox;
    TMSFMXBitmapPicker1: TTMSFMXBitmapPicker;
    Label1: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    TMSFMXColorPicker1: TTMSFMXColorPicker;
    Label3: TLabel;
    Panel2: TPanel;
    Rectangle1: TRectangle;
    Label4: TLabel;
    GroupBox3: TGroupBox;
    TMSFMXBitmapSelector1: TTMSFMXBitmapSelector;
    lblSample: TLabel;
    GroupBox4: TGroupBox;
    TMSFMXColorSelector1: TTMSFMXColorSelector;
    Label5: TLabel;
    Panel3: TPanel;
    Rectangle2: TRectangle;
    GroupBox5: TGroupBox;
    TMSFMXBitmapSelector2: TTMSFMXBitmapSelector;
    GroupBox6: TGroupBox;
    Memo1: TMemo;
    TMSFMXFontNamePicker1: TTMSFMXFontNamePicker;
    TMSFMXFontSizePicker1: TTMSFMXFontSizePicker;
    procedure TMSFMXBitmapPicker1ItemSelected(Sender: TObject;
      AItemIndex: Integer);
    procedure TMSFMXColorPicker1ItemSelected(Sender: TObject;
      AItemIndex: Integer);
    procedure TMSFMXBitmapSelector1ItemSelected(Sender: TObject;
      AItemIndex: Integer);
    procedure TMSFMXColorSelector1ItemClick(Sender: TObject;
      AItemIndex: Integer);
    procedure TMSFMXColorSelector1ItemSelected(Sender: TObject;
      AItemIndex: Integer);
    procedure TMSFMXBitmapSelector2ItemAfterDrawContent(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; AItemIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXFontNamePicker1FontNameSelected(Sender: TObject;
      AFontName: string);
    procedure TMSFMXFontSizePicker1FontSizeSelected(Sender: TObject;
      AFontSize: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.FormCreate(Sender: TObject);
begin
  TMSFMXFontSizePicker1.SelectedFontSize := Memo1.Font.Size;
  TMSFMXFontNamePicker1.SelectedFontName := Memo1.Font.Family;
end;

procedure TForm5.TMSFMXBitmapPicker1ItemSelected(Sender: TObject;
  AItemIndex: Integer);
begin
  image1.Bitmap.Assign(TMSFMXBitmapPicker1.SelectedBitmap);
end;

procedure TForm5.TMSFMXBitmapSelector1ItemSelected(Sender: TObject;
  AItemIndex: Integer);
begin
  case AItemIndex of
  0: lblSample.TextAlign := TTextAlign.taLeading;
  1: lblSample.TextAlign := TTextAlign.taCenter;
  2: lblSample.TextAlign := TTextAlign.taTrailing;
  end;
end;

procedure TForm5.TMSFMXBitmapSelector2ItemAfterDrawContent(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; AItemIndex: Integer);
var
  pt: TPathData;
begin
  case TMSFMXBitmapSelector2.Items[AItemIndex].State of
  isHover: InflateRect(ARect,-4, -4);
  isDown,isSelected:
    begin
      InflateRect(ARect,-4, -4);
      ACanvas.Stroke.Thickness := 2;
      ACanvas.Stroke.Color := claBlack;
    end;
  isNormal: InflateRect(ARect,-8, -8);
  end;

  ARect := RectF(Int(ARect.Left)+ 0.5, Int(ARect.Top) + 0.5, Int(ARect.Right) +0.5, Int(ARect.Bottom) + 0.5);

  case AItemIndex of
  0:
    begin
      ACanvas.Fill.Color := claBlue;

      ACanvas.FillEllipse(ARect,1);
      ACanvas.DrawEllipse(ARect,1);
    end;
  1:
    begin
      ACanvas.Fill.Color := claGreen;
      ACanvas.FillRect(ARect,0,0,AllCorners,1);
      ACanvas.DrawRect(ARect,0,0,AllCorners,1);
    end;
  2:
    begin
      pt := TPathData.Create;


      pt.MoveTo(PointF(ARect.Left + ARect.Width / 2, ARect.Top));
      pt.LineTo(PointF(ARect.Left + ARect.Width , ARect.Bottom));
      pt.LineTo(PointF(ARect.Left , ARect.Bottom));
      pt.ClosePath;
      //pt.LineTo(Point(ARect.Left + ARect.Width div 2, ARect.Top));

      ACanvas.Fill.Color := claRed;
      ACanvas.FillPath(pt,1);
      ACanvas.DrawPath(pt,1);

      pt.Free;

    end;
  end;
end;

procedure TForm5.TMSFMXColorPicker1ItemSelected(Sender: TObject;
  AItemIndex: Integer);
begin
  Rectangle1.Fill.Color := TMSFMXColorPicker1.SelectedColor;
end;

procedure TForm5.TMSFMXColorSelector1ItemClick(Sender: TObject;
  AItemIndex: Integer);
begin
  if AItemIndex = TMSFMXColorSelector1.Items.Count - 1 then
  begin
    ShowMessage('Pick a custom color here');
    TMSFMXColorSelector1.Items[AItemIndex].Color := claRed;
    Rectangle2.Fill.Color := claRed;
  end;
end;

procedure TForm5.TMSFMXColorSelector1ItemSelected(Sender: TObject;
  AItemIndex: Integer);
begin
  Rectangle2.Fill.Color := TMSFMXColorSelector1.SelectedColor;
end;

procedure TForm5.TMSFMXFontNamePicker1FontNameSelected(Sender: TObject;
  AFontName: string);
begin
  Memo1.Font.Family := AFontName;
end;

procedure TForm5.TMSFMXFontSizePicker1FontSizeSelected(Sender: TObject;
  AFontSize: Single);
begin
  Memo1.Font.Size := AFontSize;
end;

end.
