unit UToolBarDemo;

interface

{$IFDEF IOS}
{$DEFINE MOBILE}
{$ENDIF}
{$IFDEF ANDROID}
{$DEFINE MOBILE}
{$ENDIF}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Generics.Collections,
  FMX.TMSBitmapContainer, FMX.TMSToolBar, FMX.Objects, FMX.StdCtrls, IOUtils, UIConsts;

type
  TTMSFMXPaintElement = class(TPersistent)
  private
    FColor: TAlphaColor;
  public
    property Color: TAlphaColor read FColor write FColor;
  end;

  TTMSFMXLineElement = class(TTMSFMXPaintElement)
  private
    FWidth: Integer;
    FPoints: TList<TPointF>;
  public
    constructor Create;
    property Width: Integer read FWidth write FWidth;
    property Points: TList<TPointF> read FPoints write FPoints;
  end;

  TTMSFMXTextElement = class(TTMSFMXPaintElement)
  private
    FFontSize: Integer;
    FFontName: String;
    FPos: TPointF;
    FText: String;
  public
    property FontName: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property Pos: TPointF read FPos write FPos;
    property Text: String read FText write FText;
  end;

  TForm88 = class(TForm)
    TMSFMXToolBar1: TTMSFMXToolBar;
    PaintBox1: TPaintBox;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    TMSFMXToolBarButton1: TTMSFMXToolBarButton;
    TMSFMXToolBarButton2: TTMSFMXToolBarButton;
    TMSFMXToolBarColorPicker1: TTMSFMXToolBarColorPicker;
    TMSFMXToolBarFontSizePicker1: TTMSFMXToolBarFontSizePicker;
    TMSFMXToolBarSeparator1: TTMSFMXToolBarSeparator;
    TMSFMXToolBarButton3: TTMSFMXToolBarButton;
    TMSFMXToolBarSeparator2: TTMSFMXToolBarSeparator;
    TMSFMXToolBarFontNamePicker1: TTMSFMXToolBarFontNamePicker;
    TMSFMXToolBarFontSizePicker2: TTMSFMXToolBarFontSizePicker;
    TMSFMXToolBarButton4: TTMSFMXToolBarButton;
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TMSFMXToolBarButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure TMSFMXToolBarColorPicker1ColorSelected(Sender: TObject;
      AColor: TAlphaColor);
    procedure TMSFMXToolBarFontSizePicker1FontSizeSelected(Sender: TObject;
      AFontSize: Integer);
    procedure TMSFMXToolBarButton1Click(Sender: TObject);
    procedure TMSFMXToolBarButton3Click(Sender: TObject);
    procedure TMSFMXToolBarColorPicker1Click(Sender: TObject);
    procedure TMSFMXToolBarButton4Click(Sender: TObject);
  private
    { Private declarations }
    FStartPainting: Boolean;
    FActiveLineElement: TTMSFMXLineElement;
    FActiveTextElement: TTMSFMXTextElement;
    FPaintCol: TObjectList<TTMSFMXPaintElement>;
    FActiveColor: TAlphaColor;
    FActiveThickness: Integer;
  public
    { Public declarations }
  end;

var
  Form88: TForm88;

implementation

{$R *.fmx}

procedure TForm88.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  {$IFDEF MOBILE}
  TMSFMXToolBar1.State := esLarge;
  {$ENDIF}
  FActiveColor := claRed;
  FActiveThickness := 3;
  TMSFMXToolBarColorPicker1.SelectedColor := claRed;
  FPaintCol := TObjectList<TTMSFMXPaintElement>.Create;
  TMSFMXToolBarFontSizePicker1.ListBox.Clear;
  for I := 1 to 20 do
    TMSFMXToolBarFontSizePicker1.ListBox.Items.Insert(I, inttostr(I));

  TMSFMXToolBarFontSizePicker1.SelectedItemIndex := FActiveThickness - 1;
  {$IFDEF MACOS}
  TMSFMXToolBarFontNamePicker1.SelectedFontName := 'Helvetica';
  {$ENDIF}
  {$IFDEF ANDROID}
  TMSFMXToolBarFontNamePicker1.SelectedFontName := 'Roboto';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  TMSFMXToolBarFontNamePicker1.SelectedFontName := 'Tahoma';
  {$ENDIF}

  TMSFMXToolBarFontSizePicker2.SelectedFontSize := 18;
end;

procedure TForm88.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if TMSFMXToolBarButton4.DownState then
    Exit;

  FStartPainting := True;
  FActiveLineElement := TTMSFMXLineElement.Create;
  if TMSFMXToolBarButton3.DownState then
    FActiveLineElement.Color := claWhite
  else
    FActiveLineElement.Color := FActiveColor;
  FActiveLineElement.Width := FActiveThickness;
  FActiveLineElement.Points.Add(PointF(X, Y));
  FPaintCol.Add(FActiveLineElement);
  PaintBox1.Repaint;
end;

procedure TForm88.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FStartPainting and Assigned(FActiveLineElement) then
  begin
    FActiveLineElement.Points.Add(PointF(X, Y));
    PaintBox1.Repaint;
  end;
end;

procedure TForm88.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  str: String;
begin
  if TMSFMXToolBarButton4.DownState then
  begin
    TMSFMXToolBarButton4.DownState := False;
    FActiveTextElement := TTMSFMXTextElement.Create;
    FActiveTextElement.Color := TMSFMXToolBarColorPicker1.SelectedColor;
    FActiveTextElement.FontName := TMSFMXToolBarFontNamePicker1.SelectedFontName;
    FActiveTextElement.Pos := PointF(X, Y);
    FActiveTextElement.FontSize := TMSFMXToolBarFontSizePicker2.SelectedFontSize;
    str := 'Hello World !';
    {$IFNDEF ANDROID}
    InputQuery('Please enter text', '', str);
    {$ENDIF}
    FActiveTextElement.Text := str;
    FPaintCol.Add(FActiveTextElement);
  end;

  if Assigned(FActiveLineElement) then
    FActiveLineElement.Points.Add(PointF(X, Y));
  FStartPainting := False;
  PaintBox1.Repaint;
  FActiveLineElement := nil;
  FActiveTextElement := nil;
end;

procedure TForm88.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  I: Integer;
  el: TTMSFMXPaintElement;
  ln: TTMSFMXLineElement;
  txt: TTMSFMXTextElement;
  K: Integer;
  tw, th: Single;
  st: TCanvasSaveState;
begin
  st := Canvas.SaveState;
  Canvas.Fill.Color := claWhite;
  Canvas.FillRect(PaintBox1.BoundsRect, 0, 0, AllCorners, 1);
  if Assigned(FPaintCol) then
  begin
    for I := 0 to FPaintCol.Count - 1 do
    begin
      el := FPaintCol[I];

      if el is TTMSFMXLineElement then
      begin
        ln := (el as TTMSFMXLineElement);
        {$IF compilerversion > 26}
        Canvas.Stroke.Kind := TBrushKind.Solid;
        Canvas.Fill.Kind := TBrushKind.Solid;
        Canvas.StrokeCap := TStrokeCap.Round;
        {$ELSE}
        Canvas.Stroke.Kind := TBrushKind.bkSolid;
        Canvas.Fill.Kind := TBrushKind.bkSolid;
        Canvas.StrokeCap := TStrokeCap.scRound;
        {$ENDIF}
        Canvas.Stroke.Color := ln.Color;
        Canvas.Fill.Color := ln.Color;
        Canvas.Stroke.Thickness := ln.Width;
        {$IFDEF MSWINDOWS}
        {$ENDIF}
        if (el as TTMSFMXLineElement).Points.Count > 1 then
        begin
          for K := 0 to ln.Points.Count - 2 do
            Canvas.DrawLine(ln.Points[K], ln.Points[K + 1], 1);
        end
        else
          Canvas.FillEllipse(RectF(ln.Points[0].X - ln.Width / 2, ln.Points[0].Y - ln.Width / 2, ln.Points[0].X + ln.Width / 2, ln.Points[0].Y + ln.Width / 2), 1);
      end
      else if el is TTMSFMXTextElement then
      begin
        txt := el as TTMSFMXTextElement;
        {$IF compilerversion > 26}
        Canvas.Fill.Kind := TBrushKind.Solid;
        {$ELSE}
        Canvas.Fill.Kind := TBrushKind.bkSolid;
        {$ENDIF}
        Canvas.Font.Family := txt.FontName;
        Canvas.Fill.Color := txt.Color;
        Canvas.Font.Size := txt.FontSize;
        tw := Canvas.TextWidth(txt.Text);
        th := Canvas.TextHeight(txt.Text);
        {$IF compilerversion > 26}
        Canvas.FillText(RectF(txt.Pos.X, txt.Pos.Y, txt.Pos.X + tw, txt.Pos.Y + th), txt.Text, False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
        {$ELSE}
        Canvas.FillText(RectF(txt.Pos.X, txt.Pos.Y, txt.Pos.X + tw, txt.Pos.Y + th), txt.Text, False, 1, [], TTextAlign.taLeading, TTextAlign.taLeading);
        {$ENDIF}
      end;
    end;
  end;

  Canvas.RestoreState(st);
end;

procedure TForm88.TMSFMXToolBarButton1Click(Sender: TObject);
begin
  FPaintCol.Clear;
  PaintBox1.Repaint;
end;

procedure TForm88.TMSFMXToolBarButton2Click(Sender: TObject);
{$IFNDEF MOBILE}
var
  sd: TSaveDialog;
  str: String;
  bmp: TBitmap;
{$ENDIF}
begin
  {$IFDEF MOBILE}
  ShowMessage('ToolBar Button event handling: Drawing saved');
  {$ELSE}
  sd := TSaveDialog.Create(Self);
  sd.Filter := 'Portable Network Graphics (*.png)|*.png';
  try
    if sd.Execute then
    begin
      str := sd.FileName;
      if not UpperCase(str).Contains('.PNG') then
        str := str + '.png';

      bmp := nil;
      try
        bmp := PaintBox1.MakeScreenshot;
        if Assigned(bmp) then
        begin
          bmp.SaveToFile(str);
        end;
      finally
        if Assigned(bmp) then
          bmp.Free;
      end;
    end;
  finally
    sd.Free;
  end;
  {$ENDIF}
end;

procedure TForm88.TMSFMXToolBarButton3Click(Sender: TObject);
begin
  TMSFMXToolBarButton3.DownState := not TMSFMXToolBarButton3.DownState;
  TMSFMXToolBarButton4.DownState := False;
end;

procedure TForm88.TMSFMXToolBarButton4Click(Sender: TObject);
begin
  TMSFMXToolBarButton4.DownState := not TMSFMXToolBarButton4.DownState;
  TMSFMXToolBarButton3.DownState := False;
end;

procedure TForm88.TMSFMXToolBarColorPicker1Click(Sender: TObject);
begin
  TMSFMXToolBarButton3.DownState := False;
end;

procedure TForm88.TMSFMXToolBarColorPicker1ColorSelected(Sender: TObject;
  AColor: TAlphaColor);
begin
  FActiveColor := AColor;
end;

procedure TForm88.TMSFMXToolBarFontSizePicker1FontSizeSelected(Sender: TObject;
  AFontSize: Integer);
begin
  FActiveThickness := AFontSize;
end;

{ TTMSFMXLineElement }

constructor TTMSFMXLineElement.Create;
begin
  FPoints := TList<TPointF>.Create;
end;

end.
