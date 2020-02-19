//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit DebugDraw;

interface

uses Box2D.Common, Box2D.Collision, Box2D.Dynamics, FMX.Graphics,
     System.Math.Vectors, System.UITypes;

const
  DEFAULT_OPACITY: Double = 100;
  DEFAULT_SCALE: Single = 10.0;

type
  float32 = Single;

  PCamera = ^TCamera;
  TCamera = record
  var
    m_center: b2Vec2;
    m_extent: float32;
    m_zoom: float32;
    m_width: int32;
    m_height: int32;

    class function Create: TCamera; static;
  end;

  TNoRefCount = class(TInterfacedObject)
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TDebugDraw = class(TNoRefCount, Ib2Draw)
  private
    FCanvas: TCanvas;
    FCanvasHeight: Single;
    FScale: Single;
    FPathData: TPathData;
    FDrawHandle: b2DrawHandle;
    FDrawWrapper: b2DrawWrapper;

    procedure b2VerticesToPolygon(vertices: Pb2Vec2; vertexCount: int32;
                                  var polygon: TPolygon);
    procedure SetCanvas(const Value: TCanvas);
    procedure SetCanvasHeight(const Value: float32);  public

    constructor Create;
    destructor Destroy; override;

  public
    function ScreenToWorld(x: Single; y: Single): b2Vec2;

    // -----------------
    // Ib2Ddraw
    // -----------------

    /// Draw a closed polygon provided in CCW order.
    procedure DrawPolygon(vertices: Pb2Vec2; vertexCount: int32; const [ref] color: b2Color); cdecl;
    /// Draw a solid closed polygon provided in CCW order.
    procedure DrawSolidPolygon(vertices: Pb2Vec2; vertexCount: int32; const [ref] color: b2Color); cdecl;
    /// Draw a circle.
    procedure DrawCircle(const [ref] center: b2Vec2; radius: float32; const [ref] color: b2Color); cdecl;
    /// Draw a solid circle.
    procedure DrawSolidCircle(const [ref] center: b2Vec2; radius: float32; const [ref] axis: b2Vec2; const [ref] color: b2Color); cdecl;
    /// Draw a line segment.
    procedure DrawSegment(const [ref] p1: b2Vec2; const [ref] p2: b2Vec2; const [ref] color: b2Color); cdecl;
    /// Draw a transform. Choose your own length scale.
    procedure DrawTransform(const [ref] xf: b2Transform); cdecl;

    // -----------------
    // b2Ddraw
    // -----------------
    procedure SetFlags(flags: Cardinal); cdecl;
    function GetFlags: Cardinal; cdecl;
    procedure AppendFlags(flags: Cardinal); cdecl;
    procedure ClearFlags(flags: Cardinal); cdecl;

    procedure DrawPoint(const [ref] p: b2Vec2; size: float32; const [ref] color: b2Color);
    procedure DrawString(x: Integer; y: Integer; str: string; args: array of const); overload;
    procedure DrawString(const [ref] p: b2Vec2; str: string; args: array of const); overload;
    procedure DrawString(x: Integer; y: Integer; str: string); overload;
    procedure DrawString(const [ref] p: b2Vec2; str: string); overload;
    procedure DrawAABB(aabb: Pb2AABB; const [ref] color: b2Color);
    procedure Flush;

    property Canvas: TCanvas read FCanvas write SetCanvas;
    property Scale: float32 read FScale write FScale;
    property CanvasHeight: float32 read FCanvasHeight write SetCanvasHeight;
    property DrawHandle: b2DrawHandle read FDrawHandle;
  end;

function b2Color2TColor(const [ref] color: b2Color): TColor; inline;

var
  g_camera: TCamera;
  g_debugDraw: TDebugDraw;

const
  DRAW_STRING_NEW_LINE = 16;

implementation

uses
  System.Math, System.Types, System.SysUtils,
  FMX.Types, Box2DTypes;


function b2Color2TColor(const [ref] color: b2Color): TColor; inline;
begin
  TAlphaColorRec(Result).R := Round(255 * color.r);
  TAlphaColorRec(Result).G := Round(255 * color.g);
  TAlphaColorRec(Result).B := Round(255 * color.b);
  TAlphaColorRec(Result).A := Round(255 * color.a);
end;

{ Camera }

class function TCamera.Create: TCamera;
begin
  Result.m_center.&Set(0.0, 400.0);
  Result.m_extent := 25.0;
  Result.m_zoom := 1.0;
  Result.m_width := 1280;
  Result.m_height := 800;
end;


{ TNoRefCount }

function TNoRefCount._AddRef: Integer;
begin
{$IFDEF AUTOREFCOUNT}
  Result := inherited;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

function TNoRefCount._Release: Integer;
begin
{$IFDEF AUTOREFCOUNT}
  Result := inherited;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

{ TDebugDraw }

constructor TDebugDraw.Create;
begin
  FPathData := TPathData.Create;
  FScale := DEFAULT_SCALE;
  FDrawHandle := Create_b2Draw_delegate(Self);
  FDrawWrapper := FDrawHandle;
end;

destructor TDebugDraw.Destroy;
begin
  FPathData.Free;
  FPathData := nil;
  Destroy_b2Draw_delegate(FDrawHandle);
  inherited;
end;

procedure TDebugDraw.ClearFlags(flags: Cardinal);
begin
  FDrawWrapper.ClearFlags(flags);
end;

procedure TDebugDraw.AppendFlags(flags: Cardinal);
begin
  FDrawWrapper.AppendFlags(flags);
end;

function TDebugDraw.GetFlags: Cardinal;
begin
  Result := FDrawWrapper.GetFlags;
end;

procedure TDebugDraw.SetFlags(flags: Cardinal);
begin
  FDrawWrapper.SetFlags(flags);
end;

procedure TDebugDraw.b2VerticesToPolygon(vertices: Pb2Vec2; vertexCount: int32;
                                         var polygon: TPolygon);
var
  I: Integer;
  lVertices: Pb2Vec2;
begin
  SetLength(polygon, vertexCount+1);
  for I := 0 to vertexCount-1 do
  begin
    polygon[I].X := vertices^.x * FScale + g_camera.m_center.x;
    polygon[I].Y := FCanvasHeight - (vertices^.y * FScale + g_camera.m_center.y);
    Inc(vertices);
  end;
  polygon[vertexCount] := polygon[0];
end;

{$PointerMath on}
procedure TDebugDraw.DrawAABB(aabb: Pb2AABB; const [ref] color: b2Color);
var
  v: array[0..3] of b2Vec2;
begin
  v[0] := aabb.lowerBound;
  v[1] := b2Vec2.Create(aabb.upperBound.x, aabb.lowerBound.y);
  v[2] := aabb.upperBound;
  v[3] := b2Vec2.Create(aabb.lowerBound.x, aabb.upperBound.y);
  DrawPolygon(@v[0], 4, color);
end;

procedure TDebugDraw.DrawCircle(const [ref] center: b2Vec2; radius: float32;
                                const [ref] color: b2Color); cdecl;
var
  clr: TColor;
  P: TPointF;
  R: TRectF;
  LCenterX, LCenterY: Single;
begin
  LCenterX := g_camera.m_center.x;
  LCenterY := g_camera.m_center.y;
  clr := b2Color2TColor(color);
  P := TPointF.Create(center.x*FScale + LCenterX, FCanvasHeight - (center.y*FScale + LCenterY));
  R := TRectF.Create(p.X - radius*FScale, p.Y + radius*FScale, p.X + radius*FScale, p.Y - radius*FScale);
  FCanvas.Fill.Color := clr;
  FCanvas.Stroke.Color := clr;
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Thickness := 1;
  FCanvas.DrawEllipse(r, DEFAULT_OPACITY);
end;

procedure TDebugDraw.DrawPoint(const [ref] p: b2Vec2; size: float32; const [ref] color: b2Color);
var
  clr: TColor;
  Pt: TPointF;
  R: TRectF;
  LCenterX, LCenterY: Single;
begin
  LCenterX := g_camera.m_center.x;
  LCenterY := g_camera.m_center.y;
  clr := b2Color2TColor(color);
  Pt := TPointF.Create(p.x*FScale + LCenterX, FCanvasHeight - (p.y*FScale + LCenterY));
  R := TRectF.Create(pt.X - 1, pt.Y + 1, pt.X + 1, pt.Y -1);
  FCanvas.Stroke.Color := clr;
  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Thickness := 1;
  FCanvas.DrawEllipse(r, DEFAULT_OPACITY);
end;

procedure TDebugDraw.DrawPolygon(vertices: Pb2Vec2; vertexCount: int32;
                                 const [ref] color: b2Color);
var
  C: TColor;
  polygon: TPolygon;
  SB: TStrokeBrush;
begin
  C := b2Color2TColor(color);
  b2VerticesToPolygon(vertices, vertexCount, polygon);
  FCanvas.Fill.Color := C;
  SB := FCanvas.Stroke;
  SB.Color := C;
  SB.Kind := TBrushKind.Solid;
  SB.Thickness := 1.0;
  FCanvas.DrawPolygon(polygon, DEFAULT_OPACITY);
end;

procedure TDebugDraw.DrawSegment(const [ref] p1, p2: b2Vec2;
                                 const [ref] color: b2Color);
var
  clr: TColor;
  Pt1, Pt2: TPointF;
  LCenterX, LCenterY: Single;
begin
  LCenterX := g_camera.m_center.x;
  LCenterY := g_camera.m_center.y;
  clr := b2Color2TColor(color);
  Pt1 := TPointF.Create(p1.x*FScale + LCenterX, FCanvasHeight - (p1.y*FScale + LCenterY));
  Pt2 := TPointF.Create(p2.x*FScale + LCenterX, FCanvasHeight - (p2.y*FScale + LCenterY));
  Canvas.Fill.Color := clr;
  Canvas.Stroke.Color := clr;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawLine(pt1, pt2, DEFAULT_OPACITY);
end;

procedure TDebugDraw.DrawSolidCircle(const [ref] center: b2Vec2; radius: float32;
                                     const [ref] axis: b2Vec2;
                                     const [ref] color: b2Color);
var
  clr: TColor;
  p, pcenter: b2Vec2;
  LCenterX, LCenterY: Single;
begin
  LCenterX := g_camera.m_center.x;
  LCenterY := g_camera.m_center.y;
  clr := b2Color2TColor(color);
  p := b2Vec2.Create(axis.x*radius +center.x, axis.y*radius + center.y);
  p.x := p.x*FScale + LCenterX;
  p.y := FCanvasHeight - (p.y*FScale + LCenterY);
  pcenter := b2Vec2.Create(center.x*FScale + LCenterX, FCanvasHeight - (center.y*FScale + LCenterY));
  Canvas.DrawLine(TPointF.Create(pcenter.x, pcenter.y), TPointF.Create(p.x, p.y), DEFAULT_OPACITY);
  DrawCircle(center, radius, color);
end;

procedure TDebugDraw.DrawSolidPolygon(vertices: Pb2Vec2; vertexCount: int32;
                                      const [ref] color: b2Color);
var
  clr: TColor;
  polygon: TPolygon;
begin
  clr := b2Color2TColor(color);
  b2VerticesToPolygon(vertices, vertexCount, polygon);
  Canvas.Fill.Color := clr;
  Canvas.Stroke.Color := clr;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Thickness := 1;
  Canvas.DrawPolygon(polygon, DEFAULT_OPACITY);
  Canvas.FillPolygon(polygon, 0.1);
end;

procedure TDebugDraw.DrawString(x, y: Integer; str: string);
begin
  DrawString(x, y, str, []);
end;

procedure TDebugDraw.DrawString(const [ref] p: b2Vec2; str: string);
begin
  DrawString(p, str, []);
end;

procedure TDebugDraw.DrawString(const [ref] p: b2Vec2; str: string;
                                args: array of const);
var
  text: string;
  Pt: TPointF;
  Rect: TRectF;
  LCenterX, LCenterY: Single;
begin
  LCenterX := g_camera.m_center.x;
  LCenterY := g_camera.m_center.y;
  text := Format(str, args);
  Pt := TPointF.Create(p.x*FScale + LCenterX, FCanvasHeight - (p.y*FScale + LCenterY));
  Rect := TRectF.Create(pt.x, pt.y,
                        pt.x+FCanvas.TextWidth(text),
                        pt.y+Min(DRAW_STRING_NEW_LINE, Canvas.TextHeight(text)));
  Canvas.FillText(rect, text, false, DEFAULT_OPACITY,
                   [], TTextAlign.Leading,
                   TTextAlign.Leading);
end;

procedure TDebugDraw.DrawString(x, y: Integer; str: string;
                                args: array of const);
var
  text: string;
  Pt: TPointF;
  Rect: TRectF;
begin
  text := Format(str, args);
  Rect := TRectF.Create(x, y, x+FCanvas.TextWidth(text), y+Min(DRAW_STRING_NEW_LINE, Canvas.TextHeight(text)));
  Canvas.FillText(rect, text, false, DEFAULT_OPACITY, [], TTextAlign.Leading, TTextAlign.Leading);
end;

procedure TDebugDraw.DrawTransform(const [ref] xf: b2Transform);
const
  k_axisScale = 0.4;
var
  p1, p2: b2Vec2;
  red, green: b2Color;
begin
  red := b2Color.Create(1.0, 0.0, 0.0, 0);
  green := b2Color.Create(0.0, 1.0, 0.0, 0);
  p1 := xf.p;
  p2 := p1 + k_axisScale * xf.q.GetXAxis;
  DrawSegment(p1, p2, red);

  p2 := p1 + k_axisScale * xf.q.GetYAxis;
  DrawSegment(p1, p2, green);
end;

procedure TDebugDraw.Flush;
begin
  // NOT USED WITH FMX
end;

function TDebugDraw.ScreenToWorld(x, y: Single): b2Vec2;
begin
  Result := b2Vec2.Create((x - g_camera.m_center.x)/FScale,
                     ((FCanvasHeight-y)-g_camera.m_center.y)/FScale);
end;

procedure TDebugDraw.SetCanvas(const Value: TCanvas);
begin
  FCanvas := Value;
end;

procedure TDebugDraw.SetCanvasHeight(const Value: float32);
begin
  FCanvasHeight := Value;
end;

initialization
  g_debugDraw := TDebugDraw.Create;

finalization
  g_debugDraw.Free;
  g_debugDraw := nil;

end.
