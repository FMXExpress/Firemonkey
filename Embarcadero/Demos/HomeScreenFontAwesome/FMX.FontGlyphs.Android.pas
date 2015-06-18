{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{Copyright(c) 2013-2014 Embarcadero Technologies, Inc.}
{                                                       }
{*******************************************************}


unit FMX.FontGlyphs.Android;

interface
{$IFDEF ANDROID}

{$SCOPEDENUMS ON}

uses
  FMX.FontGlyphs, Androidapi.JNI.GraphicsContentViewText, System.IOUtils;

type
  TAndroidFontGlyphManager = class(TFontGlyphManager)
  private
    FPaint: JPaint;
    //Current metrics
    FTop: Integer;
    FAscent: Integer;
    FDescent: Integer;
    FBottom: Integer;
    FLeading: Integer;

  protected
    procedure LoadResource; override;
    procedure FreeResource; override;
    function DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph; override;
    function DoGetBaseline: Single; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;
{$ENDIF}
implementation
{$IFDEF ANDROID}
uses
  System.Types, System.Math, System.Character, System.Generics.Collections, System.UIConsts, System.UITypes,
  System.Classes, System.SysUtils, FMX.Types, FMX.Surfaces, FMX.Graphics, Androidapi.JNI.JavaTypes, Androidapi.Bitmap,
  Androidapi.JNIBridge, Androidapi.Helpers;

{ TAndroidFontGlyphManager }

constructor TAndroidFontGlyphManager.Create;
begin
  inherited Create;
  FPaint := TJPaint.Create;
end;

destructor TAndroidFontGlyphManager.Destroy;
begin
  FPaint := nil;
  inherited;
end;

procedure TAndroidFontGlyphManager.LoadResource;
const
  BoldAndItalic = [TFontStyle.fsBold, TFontStyle.fsItalic];
var
  TypefaceFlag: Integer;
  Typeface: JTypeface;
  FamilyName: JString;
  Metrics: JPaint_FontMetricsInt;
  FontFile: string;
begin
  FPaint.setAntiAlias(True);
  FPaint.setTextSize(CurrentSettings.Size * CurrentSettings.Scale);
  FPaint.setARGB(255, 255, 255, 255);
  if TOSVersion.Check(4, 0) then
    FPaint.setHinting(TJPaint.JavaClass.HINTING_ON);
  //Font
  try
    FamilyName := StringToJString(CurrentSettings.Family);
    if (BoldAndItalic * CurrentSettings.Style) = BoldAndItalic then
      TypefaceFlag := TJTypeface.JavaClass.BOLD_ITALIC
    else
      if TFontStyle.fsBold in CurrentSettings.Style then
        TypefaceFlag := TJTypeface.JavaClass.BOLD
      else
        if TFontStyle.fsItalic in CurrentSettings.Style then
          TypefaceFlag := TJTypeface.JavaClass.ITALIC
        else
          TypefaceFlag := TJTypeface.JavaClass.NORMAL;
    //Typeface := TJTypeface.JavaClass.create(FamilyName, TypefaceFlag);

    FontFile := TPath.GetDocumentsPath + PathDelim + CurrentSettings.Family + '.ttf';
    if FileExists(FontFile) then
      Typeface := TJTypeface.JavaClass.createFromFile(StringToJString(FontFile))
    else
      Typeface := TJTypeface.JavaClass.Create(FamilyName, TypefaceFlag);


    FPaint.setTypeface(Typeface);
    try
      Metrics := FPaint.getFontMetricsInt;
      //
      FTop := Metrics.top;
      FAscent := Metrics.ascent;
      FDescent := Metrics.descent;
      FBottom := Metrics.bottom;
      FLeading := Metrics.leading;
    finally
      Metrics := nil;
    end;
  finally
    FamilyName := nil;
    Typeface := nil;
  end;
end;

procedure TAndroidFontGlyphManager.FreeResource;
begin
  if FPaint <> nil then
    FPaint.reset;
end;

function TAndroidFontGlyphManager.DoGetBaseline: Single;
begin
  Result := Abs(FAscent);
end;

function TAndroidFontGlyphManager.DoGetGlyph(const Char: UCS4Char; const Settings: TFontGlyphSettings): TFontGlyph;
var
  Text: JString;
  Bitmap: JBitmap;
  Canvas: JCanvas;
  GlyphRect: TRect;
  C, I, J, Width, Height, OriginY: Integer;
  Advance: Single;
  Bounds: JRect;
  GlyphStyle: TFontGlyphStyles;
  PixelBuffer: Pointer;
  Data: PIntegerArray;
  Path: JPath;
  PathMeasure: JPathMeasure;
  PathLength: Single;
  Coords: TJavaArray<Single>;
  StartPoint, LastPoint, Point: TPointF;
  NewContour, HasStartPoint: Boolean;
begin
  Text := StringToJString(System.Char.ConvertFromUtf32(Char));
  try
    Advance := FPaint.measureText(Text);
    Height := Abs(FTop) + Abs(FBottom) + 2;
    Width := Ceil(Abs(Advance)) + 2;
    Bounds := TJRect.Create;
    try
      FPaint.getTextBounds(Text, 0, Text.length, Bounds);
      if Bounds.left < 0 then
        Width := Width - Bounds.left;
      Bitmap := TJBitmap.JavaClass.createBitmap(Width, Height, TJBitmap_Config.JavaClass.ARGB_8888);
      try
        Canvas := TJCanvas.JavaClass.init(Bitmap);
        try
          if Bounds.left < 0 then
            Canvas.drawText(Text, -Bounds.left, -FAscent, FPaint)
          else
            Canvas.drawText(Text, 0, -FAscent, FPaint);
        finally
          Canvas := nil;
        end;

        GlyphStyle := [];
        if ((FAscent = 0) and (FDescent = 0)) or not HasGlyph(Char) then
          GlyphStyle := [TFontGlyphStyle.NoGlyph];
        if TFontGlyphSetting.Path in Settings then
          GlyphStyle := GlyphStyle + [TFontGlyphStyle.HasPath];

        // For some font sizes Ascent line is below Bounds.top, cuting off part of a glyph.
        // Do not use Y-value of the origin point in such cases.
        if FAscent > Bounds.top then
          OriginY := 0
        else
          OriginY := Abs(FAscent - Bounds.top);
        Result := TFontGlyph.Create(TPoint.Create(Bounds.left, OriginY), Advance,
          Abs(FAscent) + Abs(FDescent) + Abs(FLeading), GlyphStyle);

        if (TFontGlyphSetting.Bitmap in Settings) and (HasGlyph(Char) or ((FAscent <> 0) or (FDescent <> 0))) and
           (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (Bitmap as ILocalObject).GetObjectID, @PixelBuffer) = 0) then
        begin
          Data := PIntegerArray(PixelBuffer);
          GlyphRect.Left := Bounds.left;
          GlyphRect.Right := Bounds.Right;
          GlyphRect.Top := OriginY;
          GlyphRect.Bottom := Abs(FAscent - Bounds.bottom);

          if (GlyphRect.Width > 0) or (GlyphRect.Height > 0) then
          begin
            Result.Bitmap.SetSize(GlyphRect.Width + 1, GlyphRect.Height + 1, TPixelFormat.BGRA);
            if TFontGlyphSetting.PremultipliedAlpha in Settings then
            begin
              for I := GlyphRect.Top to GlyphRect.Bottom do
                Move(Data[I * Width + Max(GlyphRect.Left, 0)],
                  Result.Bitmap.GetPixelAddr(0, I - GlyphRect.Top)^, Result.Bitmap.Pitch);
            end
            else
              for I := GlyphRect.Top to GlyphRect.Bottom - 1 do
                for J := GlyphRect.Left to GlyphRect.Right - 1 do
                begin
                  C := Data[I * Width + J];
                  if C <> 0 then
                  begin
                    C := ((C shr 16) and $FF + (C shr 8) and $FF + (C and $FF)) div 3;
                    Result.Bitmap.Pixels[J - GlyphRect.Left, I - GlyphRect.Top] := MakeColor($FF, $FF, $FF, C);
                  end
                end;
          end;
          AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (Bitmap as ILocalObject).GetObjectID);
        end;
        //Path
        if TFontGlyphSetting.Path in Settings then
        try
          Path := TJPath.Create;
          FPaint.getTextPath(Text, 0, Text.length, Result.Origin.X, Result.Origin.Y, Path);
          PathMeasure := TJPathMeasure.Create;
          PathMeasure.setPath(Path, False);
          Coords := TJavaArray<Single>.Create(2);
          if PathMeasure.getLength > 0 then
          repeat
            PathLength := PathMeasure.getLength;
            NewContour := True;
            HasStartPoint := False;
            I := 0;
            while I < PathLength do
            begin
              if PathMeasure.getPosTan(I, Coords, nil) then
              begin
                Point := PointF(Coords[0], Coords[1]);
                if NewContour then
                begin
                  Result.Path.MoveTo(Point);
                  NewContour := False;
                  HasStartPoint := False;
                end
                else
                  if Point <> LastPoint then
                  begin
                    if HasStartPoint and (LastPoint <> StartPoint) then
                      if not SameValue(((Point.Y - StartPoint.Y) / (Point.X - StartPoint.X)), ((Point.Y - LastPoint.Y) / (Point.X - LastPoint.X)), Epsilon) then
                      begin
                        Result.Path.LineTo(Point);
                        HasStartPoint := False;
                      end
                      else
                    else
                      Result.Path.LineTo(Point);
                  end;
                LastPoint := Point;
                if not HasStartPoint then
                begin
                  StartPoint := Point;
                  HasStartPoint := True;
                end;
              end;
              Inc(I);
            end;
            if Result.Path.Count > 0 then
              Result.Path.ClosePath;
          until not PathMeasure.nextContour;
          Point := Result.Path.GetBounds.TopLeft;
          Result.Path.Translate(-Point.X + Result.Origin.X, -Point.Y + Result.Origin.Y);
        finally
          FreeAndNil(Coords);
          Path := nil;
          PathMeasure := nil;
        end;
      finally
        Bitmap.recycle;
        Bitmap := nil;
      end;
    finally
      Bounds := nil;
    end;
  finally
    Text := nil;
  end;
end;
{$ENDIF}
end.

