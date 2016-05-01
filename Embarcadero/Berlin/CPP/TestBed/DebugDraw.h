//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef DebugDrawH
#define DebugDrawH

#include <Box2D/Box2D.h>
#include <FMX.Graphics.hpp>

// Used partially by FMX TestBed. 
// NOTE: Some tests refer to 'g_camera'
struct Camera
{
  Camera()
  {
    m_center.Set(0.0f, 20.0f);
    m_extent = 25.0f;
    m_zoom = 1.0f;
    m_width = 1280;
    m_height = 800;
  }

  b2Vec2 m_center;
  float32 m_extent;
  float32 m_zoom;
  int32 m_width;
  int32 m_height;
};

class TDebugDraw : public b2Draw
{
  TCanvas* FCanvas;
  /* TODO : Merge Camera and TDebugDraw state */
  float FScaleX, FScaleY;
  float FCanvasHeight;
  void SetCanvas(TCanvas* canvas);
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
const double DEFAULT_SCALE = 12.5;

#endif
