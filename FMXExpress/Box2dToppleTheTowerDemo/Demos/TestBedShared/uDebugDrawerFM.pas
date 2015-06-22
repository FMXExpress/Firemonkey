unit uDebugDrawerFM;

interface

uses
  System.UITypes, UPhysics2DTypes, UPhysics2D, uDebugDrawer, System.Types, FMX.Graphics
{$IFDEF VER270}
 , System.Math.Vectors
{$ENDIF}
  ;

const
  DEFAULT_OPACITY = 1;

type
  TDebugDrawerFM = class(TDebugDrawer)
  private
    FCanvas: TCanvas;
    FOffsetX, FOffsetY: Double;
    FScaleX, FScaleY: Double;
    FCanvasHeight: Double;
    GPath: TPathData;
    procedure SetCanvas(const Value: TCanvas);
    procedure SetOffsetX(const Value: Double);
    procedure SetOffsetY(const Value: Double);
    procedure SetScaleX(const Value: Double);
    procedure SetScaleY(const Value: Double);
    procedure SetCanvasHeight(const Value: Double);
    procedure DrawEllipse(Canvas: TCanvas; aRect: TRectF; aOpacity: Single);
  public
    constructor Create;
    destructor Destroy; override;
    function ConvertScreenToWorld(x,y: Single): uPhysics2DTypes.TVector2;

    // uPhysics2D.Tb2Draw overrides
    procedure DrawPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA); override;
    procedure DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSegment(const p1, p2: TVector2; const color: RGBA); override;
    procedure DrawTransform(const xf: Tb2Transform); override;

    // uDebugDrawer.TDebugDrawer overrides
    procedure DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA); override;
    procedure DrawAABB(const aabb: Tb2AABB; const color: RGBA); override;
    procedure SetDefaultFontColor(const Value: TColor); override;
    procedure SetCanvasTranslation(x, y: PhysicsFloat); override;
    procedure SetCanvasTranslationOffset(dx, dy: PhysicsFloat); override;
    procedure TextOutASCII(text: string; x,y: integer); override;
  public
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property OffsetX: Double read FOffsetX write SetOffsetX;
    property OffsetY: Double read FOffsetY write SetOffsetY;
    property ScaleX: Double read FScaleX write SetScaleX;
    property ScaleY: Double read FScaleY write SetScaleY;
    property CanvasHeight: Double read FCanvasHeight write SetCanvasHeight;
  end;

implementation

uses
  FMX.Types, uFMDrawUtils;

{ TDebugDrawerFM }

function TDebugDrawerFM.ConvertScreenToWorld(x,
  y: Single): uPhysics2DTypes.TVector2;
begin
  Result.x := (x - offsetX) / scaleX;
  Result.y := ((CanvasHeight-y) - offsetY) / scaleY;
end;

constructor TDebugDrawerFM.Create;
begin
  inherited;
  FScaleX := 10;
  FScaleY := 10;
  FOffsetX := 0;
  FOffsetY := 0;
  GPath := TPathData.Create;
end;

destructor TDebugDrawerFM.Destroy;
begin
  GPath.Free;
  inherited;
end;


procedure TDebugDrawerFM.SetCanvas(const Value: TCanvas);
begin
  FCanvas := Value;
end;

procedure TDebugDrawerFM.SetCanvasHeight(const Value: Double);
begin
  FCanvasHeight := Value;
end;

procedure TDebugDrawerFM.SetOffsetX(const Value: Double);
begin
  FOffsetX := Value;
end;

procedure TDebugDrawerFM.SetOffsetY(const Value: Double);
begin
  FOffsetY := Value;
end;

procedure TDebugDrawerFM.SetScaleX(const Value: Double);
begin
  FScaleX := Value;
end;

procedure TDebugDrawerFM.SetScaleY(const Value: Double);
begin
  FScaleY := Value;
end;

procedure TDebugDrawerFM.DrawAABB(const aabb: Tb2AABB; const color: RGBA);
var pv: Tb2PolyVertices;
begin
  pv := AABBToPolyVertices(aabb);
  DrawPolygon(pv,4,color);
end;

procedure TDebugDrawerFM.DrawEllipse(Canvas: TCanvas; aRect: TRectF; aOpacity: Single);
begin
  // work around so ellipses don't crash on mobile
  GPath.Clear;
  GPath.AddEllipse(aRect);
  Canvas.DrawPath(GPath, aOpacity);
end;

procedure TDebugDrawerFM.DrawPoint(const p: TVector2; size: PhysicsFloat;
  const color: RGBA);
var aColor: TColor; aRect: TRectF; r: TPointF;
begin
  aColor := RGBAToFMColor(color);

  r.X := p.x * scaleX + offsetX;
  r.Y := CanvasHeight - (p.y * scaleY + offsetY);
  aRect.Left := r.X - 1;
  aRect.Top := r.Y + 1;
  aRect.Right := r.X + 1;
  aRect.Bottom := r.Y - 1;

  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;

  //DrawEllipse() crashes on Mobile
  //Canvas.DrawEllipse(aRect, DEFAULT_OPACITY);
  DrawEllipse(Canvas, aRect, DEFAULT_OPACITY);
end;

procedure TDebugDrawerFM.DrawPolygon(const vertices: Tb2PolyVertices;
  vertexCount: Int32; const color: RGBA);
var polygon: TPolygon; aColor: TColor;
begin
  aColor := RGBAToFMColor(color);
  b2PolyVerticesToPolygon(vertices, vertexCount, polygon, offsetX, offsetY, scaleX, scaleY, True, CanvasHeight);
  Canvas.Fill.Color := acolor;
  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawPolygon(polygon, DEFAULT_OPACITY);
end;

procedure TDebugDrawerFM.DrawPolygon4(const vertices: TVectorArray4;
  vertexCount: Int32; const color: RGBA);
var pv: Tb2PolyVertices;
begin
  pv := VectorArray4ToPolyVertices(vertices);
  DrawPolygon(pv, vertexCount, color);
end;

procedure TDebugDrawerFM.DrawSegment(const p1, p2: TVector2; const color: RGBA);
var aColor: TColor; aP1, aP2: TPointF;
begin
  aColor := RGBAToFMColor(color);
  aP1.X := offsetX + p1.x * scaleX;
  aP1.Y := CanvasHeight - (offsetY + p1.y * scaleY);
  aP2.X := offsetX + p2.x * scaleX;
  aP2.Y := CanvasHeight - (offsetY + p2.y * scaleY);
  Canvas.Fill.Color := aColor;
  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(aP1, aP2, DEFAULT_OPACITY);
end;

procedure TDebugDrawerFM.DrawSolidCircle(const center, axis: TVector2;
  radius: PhysicsFloat; const color: RGBA);
var
  p, pcenter: TVector2;
  tmp: RGBA;
  aColor: TColor; aRect: TRectF;
begin
  aColor := RGBAToFMColor(color);

  {$IFDEF OP_OVERLOAD}
  p := center + radius * axis;
  {$ELSE}
  p.x := axis.x * radius + center.x;
  p.y := axis.y * radius + center.y;
  {$ENDIF}

  p.x := offsetX + p.x * scaleX;
  p.y := CanvasHeight - (offsetY + p.y * scaleY);

  pcenter.x := offsetX + center.x * scaleX;
  pcenter.y := CanvasHeight - (offsetY + center.y * scaleY);

  Canvas.Fill.Color := aColor;
  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(PointF(pcenter.x, pcenter.y), PointF(p.x, p.y), DEFAULT_OPACITY);

  DrawCircle(center,radius,color);
end;

procedure TDebugDrawerFM.DrawCircle(const center: TVector2;
  radius: PhysicsFloat; const color: RGBA);
var aColor: TColor; aRect: TRectF; p: TPointF; r: Double;
begin
  aColor := RGBAToFMColor(color);

  p.X := center.x * scaleX + offsetX;
  p.Y := CanvasHeight - (center.y * scaleY + offsetY);
  aRect.Left := p.X - radius * scaleX;
  aRect.Top := p.Y + radius * scaleY;
  aRect.Right := p.X + radius * scaleX;
  aRect.Bottom := p.Y - radius * scaleY;

  Canvas.Fill.Color := aColor;
  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;

  //DrawEllipse() crashes on Mobile
  //Canvas.DrawEllipse(aRect, DEFAULT_OPACITY);
  DrawEllipse(Canvas, aRect, DEFAULT_OPACITY);
end;

procedure TDebugDrawerFM.DrawSolidPolygon(const vertices: Tb2PolyVertices;
  vertexCount: Int32; const color: RGBA);
var polygon: TPolygon; aColor: TColor;
begin
  aColor := RGBAToFMColor(color);
  b2PolyVerticesToPolygon(vertices, vertexCount, polygon,
    offsetX, offsetY, scaleX, scaleY, True, CanvasHeight);
  Canvas.Fill.Color := acolor;
  Canvas.Stroke.Color := aColor;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawPolygon(polygon, DEFAULT_OPACITY);
  Canvas.FillPolygon(polygon, 0.1);
end;

procedure TDebugDrawerFM.DrawTransform(const xf: Tb2Transform);
const
   k_axisScale = 0.4;
var
   p1, p2: TPointF;
   xAxis, yAxis: TVector2;
begin
  {$IFDEF OP_OVERLOAD}
  xAxis := xf.q.GetXAxis;
  yAxis := xf.q.GetYAxis;
  {$ELSE}
  {$IFDEF BETTER_PERFORMANCE}
  xAxis := xf.q.GetXAxis;
  yAxis := xf.q.GetYAxis;
  {$ELSE}
  xAxis := GetXAxis(xf.q);
  yAxis := GetYAxis(xf.q);
  {$ENDIF}
  {$ENDIF}

  p2.x := xf.p.x + k_axisScale * xAxis.x;
  p2.y := xf.p.y + k_axisScale * xAxis.y;
  p1.X := xf.p.x;
  p1.Y := xf.p.y;

  p1.x := offsetX + p1.x * scaleX;
  p1.y := offsetY + p1.y * scaleY;
  p2.x := offsetX + p2.x * scaleX;
  p2.y := offsetY + p2.y * scaleY;

  Canvas.Stroke.Color := TAlphaColorRec.Green;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(p1,p2, DEFAULT_OPACITY);

  Canvas.Stroke.Color := TAlphaColorRec.Red;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(p1,p2, DEFAULT_OPACITY);
end;

procedure TDebugDrawerFM.SetCanvasTranslation(x, y: PhysicsFloat);
begin
  FOffsetX := x;
  FOffsetY := y;
end;

procedure TDebugDrawerFM.SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
begin
  FOffsetX := FOffsetX + dx;
  FOffsetY := FOffsetY + dy;
end;

procedure TDebugDrawerFM.SetDefaultFontColor(const Value: TColor);
begin
  Canvas.Fill.Color := Value;
end;

procedure TDebugDrawerFM.TextOutASCII(text: string; x, y: integer);
var ARect: TRectF;
begin
  ARect := RectF(x, y, x + Canvas.TextWidth(text), y + Canvas.TextHeight(text));
  Canvas.FillText(ARect, text, false, DEFAULT_OPACITY, [],
          TTextAlign.taLeading, TTextAlign.taLeading);
end;

end.
