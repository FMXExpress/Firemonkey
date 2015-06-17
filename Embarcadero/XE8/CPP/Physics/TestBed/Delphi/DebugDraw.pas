unit DebugDraw;

interface

uses
  Box2DTypes, Box2D.Common;

//// Not used by FMX TestBed. Declared as some tests refer to 'g_camera'
//struct Camera
//{
//  Camera()
//  {
//    m_center.Set(0.0f, 20.0f);
//    m_extent = 25.0f;
//    m_zoom = 1.0f;
//    m_width = 1280;
//    m_height = 800;
//  }
//
//  b2Vec2 m_center;
//  float32 m_extent;
//  float32 m_zoom;
//  int32 m_width;
//  int32 m_height;
//};
type
  TCamera = record
//  Camera()
//  {
//    m_center.Set(0.0f, 20.0f);
//    m_extent = 25.0f;
//    m_zoom = 1.0f;
//    m_width = 1280;
//    m_height = 800;
//  }
    m_center: b2Vec2;
    m_extent: single;
    m_zoom: single;
    m_width: Int32;
    m_height: Int32;

    class function Create: TCamera; static;

  end;

(*
class TDebugDraw : public b2Draw
{
  TCanvas* FCanvas;
  /* TODO : Merge Camera and TDebugDraw state */
  float FOffsetX, FOffsetY;
  float FScaleX, FScaleY;
  float FCanvasHeight;
  TPathData* GPath;
  void SetCanvas(TCanvas* canvas);
  void SetOffsetX(float offset);
  void SetOffsetY(float offset);
  void SetScaleX(float scale);
  void SetScaleY(float scale);
  void SetCanvasHeight(float height);
  void b2VerticesToPolygon(const b2Vec2* vertices, int32 vertexCount,
                           TPolygon &polygon);
public:
  TDebugDraw();
  ~TDebugDraw();
  b2Vec2 ScreenToWorld(float x, float y) const;

  // b2Draw
  void DrawPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color);
  void DrawSolidPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color);
  void DrawCircle(const b2Vec2& center, float32 radius, const b2Color& color);
  void DrawSolidCircle(const b2Vec2& center, float32 radius, const b2Vec2& axis, const b2Color& color);
  void DrawSegment(const b2Vec2& p1, const b2Vec2& p2, const b2Color& color);
  void DrawTransform(const b2Transform& xf);

  // Used by Tests
  void DrawPoint(const b2Vec2& p, float32 size, const b2Color& color);
  void DrawString(int x, int y, const char* string, ...);
  void DrawString(const b2Vec2& p, const char* string, ...);
  void DrawAABB(b2AABB* aabb, const b2Color& color);
  void Flush();

  __property TCanvas* Canvas = {read=FCanvas, write=SetCanvas};
  __property float OffsetX = {read=FOffsetX, write=SetOffsetX};
  __property float OffsetY = {read=FOffsetY, write=SetOffsetY};
  __property float ScaleX = {read=FScaleX, write=SetScaleX};
  __property float ScaleY = {read=FScaleY, write=SetScaleY};
  __property float CanvasHeight = {read=FCanvasHeight, write=SetCanvasHeight};

private:
  TDebugDraw(const TDebugDraw&);
  TDebugDraw& operator=(const TDebugDraw&);
};

extern TDebugDraw g_debugDraw;
extern Camera g_camera;

const double DEFAULT_OPACITY = 100;

*)

implementation

(*
TDebugDraw g_debugDraw;
Camera g_camera;

TDebugDraw::TDebugDraw() :FCanvas(0), FOffsetX(0), FOffsetY(0),
                          FScaleX(10), FScaleY(10), FCanvasHeight(0)
{
  GPath = new TPathData();
}

TDebugDraw::~TDebugDraw()
{
  delete GPath;
}

void TDebugDraw::SetCanvas(TCanvas* canvas)
{
  this->FCanvas = canvas;
}

void TDebugDraw::SetOffsetX(float offset)
{
  this->FOffsetX = offset;
}

void TDebugDraw::SetOffsetY(float offset)
{
  this->FOffsetY = offset;
}

void TDebugDraw::SetScaleX(float scale)
{
  this->FScaleX = scale;
}

void TDebugDraw::SetScaleY(float scale)
{
  this->FScaleY = scale;
}

void TDebugDraw::SetCanvasHeight(float height)
{
  this->FCanvasHeight = height;
}


b2Vec2 TDebugDraw::ScreenToWorld(float x, float y) const
{
  return b2Vec2((x - FOffsetX)/FScaleX,
                ((FCanvasHeight-y)-FOffsetY)/FScaleY);
}

void TDebugDraw::b2VerticesToPolygon(const b2Vec2* vertices, int32 vertexCount,
                                     TPolygon &polygon)
{
  polygon.Length = vertexCount+1;
  for(int i=0; i<vertexCount; ++i)
  {
    polygon[i].X = vertices[i].x*FScaleX + FOffsetX;
    polygon[i].Y = FCanvasHeight - (vertices[i].y*FScaleY + FOffsetY);
  }
  polygon[vertexCount] = polygon[0];
}

void TDebugDraw::DrawPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  TPolygon polygon;
  b2VerticesToPolygon(vertices, vertexCount, polygon);
  Canvas->Fill->Color = clr;
  Canvas->Stroke->Color = clr;
  Canvas->Stroke->Kind = TBrushKind::Solid;
  Canvas->Stroke->Thickness = 1;
  Canvas->DrawPolygon(polygon, DEFAULT_OPACITY);
}

/// Draw a solid closed polygon provided in CCW order.
void TDebugDraw::DrawSolidPolygon(const b2Vec2* vertices, int32 vertexCount, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  TPolygon polygon;
  b2VerticesToPolygon(vertices, vertexCount, polygon);
  Canvas->Fill->Color = clr;
  Canvas->Stroke->Color = clr;
  Canvas->Stroke->Kind = TBrushKind::Solid;
  Canvas->Stroke->Thickness = 1;
  Canvas->DrawPolygon(polygon, DEFAULT_OPACITY);
  Canvas->FillPolygon(polygon, 0.1);
}

/// Draw a circle.
void TDebugDraw::DrawCircle(const b2Vec2& center, float32 radius, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  TPointF p(center.x*FScaleX + FOffsetX,
            FCanvasHeight - (center.y*FScaleY + FOffsetY));
  TRectF r(p.X - radius*FScaleX,
           p.Y + radius*FScaleY,
           p.X + radius*FScaleX,
           p.Y - radius*FScaleY);
  Canvas->Fill->Color = clr;
  Canvas->Stroke->Color = clr;
  Canvas->Stroke->Kind = TBrushKind::Solid;
  Canvas->Stroke->Thickness = 1;
  Canvas->DrawEllipse(r, DEFAULT_OPACITY);
}

/// Draw a solid circle.
void TDebugDraw::DrawSolidCircle(const b2Vec2& center, float32 radius, const b2Vec2& axis, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  b2Vec2 p(axis.x*radius +center.x, axis.y*radius + center.y);
  p.x = p.x*FScaleX + FOffsetX;
  p.y = FCanvasHeight - (p.y*FScaleY + FOffsetY);
  b2Vec2 pcenter(center.x*FScaleX + FOffsetX,
                 FCanvasHeight - (center.y*FScaleY + FOffsetY));
  Canvas->DrawLine(TPointF(pcenter.x, pcenter.y),
                   TPointF(p.x, p.y), DEFAULT_OPACITY);
  DrawCircle(center, radius, color);
}

/// Draw a line segment.
void TDebugDraw::DrawSegment(const b2Vec2& p1, const b2Vec2& p2, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  TPointF pt1(p1.x*FScaleX + FOffsetX,
              FCanvasHeight - (p1.y*FScaleY + FOffsetY));
  TPointF pt2(p2.x*FScaleX + FOffsetX,
              FCanvasHeight - (p2.y*FScaleY + FOffsetY));
  Canvas->Fill->Color = clr;
  Canvas->Stroke->Color = clr;
  Canvas->Stroke->Kind = TBrushKind::Solid;
  Canvas->Stroke->Thickness = 1;
  Canvas->DrawLine(pt1, pt2, DEFAULT_OPACITY);
}

/// Draw a transform. Choose your own length scale.
/// @param xf a transform.
void TDebugDraw::DrawTransform(const b2Transform& xf)
{
  const float32 k_axisScale = 0.4f;
  b2Color red(1.0f, 0.0f, 0.0f);
  b2Color green(0.0f, 1.0f, 0.0f);
  b2Vec2 p1 = xf.p;
  b2Vec2 p2 = p1 + k_axisScale * xf.q.GetXAxis();
  DrawSegment(p1, p2, red);

  p2 = p1 + k_axisScale * xf.q.GetYAxis();
  DrawSegment(p1, p2, green);
}

void TDebugDraw::DrawPoint(const b2Vec2& p, float32 size, const b2Color& color)
{
  TColor clr = b2Color2TColor(color);
  TPointF pt(p.x*FScaleX + FOffsetX,
             FCanvasHeight - (p.y*FScaleY + FOffsetY));
  TRectF r(pt.X - 1, pt.Y + 1,
           pt.X + 1, pt.Y -1);
  Canvas->Stroke->Color = clr;
  Canvas->Stroke->Kind = TBrushKind::Solid;
  Canvas->Stroke->Thickness = 1;
  Canvas->DrawEllipse(r, DEFAULT_OPACITY);
}

void TDebugDraw::DrawString(int x, int y, const char* string, ...)
{
  char buffer[128];

  va_list arg;
  va_start(arg, string);
  vsprintf(buffer, string, arg);
  va_end(arg);
  String text(buffer);
  TRectF rect(x, y,
              x+FCanvas->TextWidth(text),
              y+b2Min(static_cast<float>(DRAW_STRING_NEW_LINE), Canvas->TextHeight(text)));
  Canvas->FillText(rect, text, false, DEFAULT_OPACITY,
                   TFillTextFlags(), TTextAlign::Leading,
                   TTextAlign::Leading);
}

void TDebugDraw::DrawString(const b2Vec2& p, const char* string, ...)
{
  char buffer[128];

  va_list arg;
  va_start(arg, string);
  vsprintf(buffer, string, arg);
  va_end(arg);
  String text(buffer);
  TPointF pt(p.x*FScaleX + FOffsetX,
             FCanvasHeight - (p.y*FScaleY + FOffsetY));
  TRectF rect(pt.x, pt.y,
              pt.x+FCanvas->TextWidth(text),
              pt.y+b2Min(static_cast<float>(DRAW_STRING_NEW_LINE), Canvas->TextHeight(text)));
  Canvas->FillText(rect, text, false, DEFAULT_OPACITY,
                   TFillTextFlags(), TTextAlign::Leading,
                   TTextAlign::Leading);
}

void TDebugDraw::DrawAABB(b2AABB* aabb, const b2Color& color)
{
  b2Vec2 v[4];
  v[0] = aabb->lowerBound;
  v[1] = b2Vec2(aabb->upperBound.x, aabb->lowerBound.y);
  v[2] = aabb->upperBound;
  v[3] = b2Vec2(aabb->lowerBound.x, aabb->upperBound.y);
  DrawPolygon(v, 4, color);
}

void TDebugDraw::Flush()
{
}

*)


{ TCamera }

class function TCamera.Create: TCamera;
begin
  Result.m_center := b2Vec2.Create(0.0, 20.0);
  Result.m_extent := 25.0;
  Result.m_zoom := 1.0;
  Result.m_width := 1280;
  Result.m_height := 800;
end;

end.
