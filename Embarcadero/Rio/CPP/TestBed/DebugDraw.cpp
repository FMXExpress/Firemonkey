//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include "DebugDraw.h"
#include "DrawUtil.h"
#include "Test.h"

// NOTE: There's no auto-linking support for Mobile
// Add "FlatBox2D" (i.e. libFlatBox2D.a) to the
// 'Additionally linked libraries" project linker option;
// We add here pragmas for Win32 target (_WIN32 is also defined for Win64)
// and OSX.
#if defined(_WIN32)
 #pragma comment(lib, "Box2D")
#elif defined(_PLAT_MACOS)
 #pragma link "libBox2D.a"
#endif

TDebugDraw g_debugDraw;
Camera g_camera;

TDebugDraw::TDebugDraw() : FCanvas(0), FScaleX(DEFAULT_SCALE), FScaleY(DEFAULT_SCALE), FCanvasHeight(0)
{}

TDebugDraw::~TDebugDraw()
{}

void TDebugDraw::SetCanvas(TCanvas* canvas)
{
  this->FCanvas = canvas;
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
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  return b2Vec2((x - LCenterX)/FScaleX,
                ((FCanvasHeight-y)-LCenterY)/FScaleY);
}

void TDebugDraw::b2VerticesToPolygon(const b2Vec2* vertices, int32 vertexCount,
                                     TPolygon &polygon)
{
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  polygon.Length = vertexCount+1;
  for(int i=0; i<vertexCount; ++i)
  {
    polygon[i].X = vertices[i].x*FScaleX + LCenterX;
    polygon[i].Y = FCanvasHeight - (vertices[i].y*FScaleY + LCenterY);
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
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  TColor clr = b2Color2TColor(color);
  TPointF p(center.x*FScaleX + LCenterX,
            FCanvasHeight - (center.y*FScaleY + LCenterY));
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
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  b2Vec2 p(axis.x*radius +center.x, axis.y*radius + center.y);
  p.x = p.x*FScaleX + LCenterX;
  p.y = FCanvasHeight - (p.y*FScaleY + LCenterY);
  b2Vec2 pcenter(center.x*FScaleX + LCenterX,
                 FCanvasHeight - (center.y*FScaleY + LCenterY));
  Canvas->DrawLine(TPointF(pcenter.x, pcenter.y),
                   TPointF(p.x, p.y), DEFAULT_OPACITY);
  DrawCircle(center, radius, color);
}

/// Draw a line segment.
void TDebugDraw::DrawSegment(const b2Vec2& p1, const b2Vec2& p2, const b2Color& color)
{
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  TColor clr = b2Color2TColor(color);
  TPointF pt1(p1.x*FScaleX + LCenterX,
              FCanvasHeight - (p1.y*FScaleY + LCenterY));
  TPointF pt2(p2.x*FScaleX + LCenterX,
              FCanvasHeight - (p2.y*FScaleY + LCenterY));
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
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  TColor clr = b2Color2TColor(color);
  TPointF pt(p.x*FScaleX + LCenterX,
             FCanvasHeight - (p.y*FScaleY + LCenterY));
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
  float& LCenterX = g_camera.m_center.x;
  float& LCenterY = g_camera.m_center.y;

  char buffer[128];

  va_list arg;
  va_start(arg, string);
  vsprintf(buffer, string, arg);
  va_end(arg);
  String text(buffer);
  TPointF pt(p.x*FScaleX + LCenterX,
             FCanvasHeight - (p.y*FScaleY + LCenterY));
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
  // NOP
}


