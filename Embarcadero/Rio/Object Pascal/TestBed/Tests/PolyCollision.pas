//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit PolyCollision;

interface

uses
  Box2D.Common, Box2D.Collision, Test;

type
  TPolyCollision = class(TTest)
  protected
  const
    b2_maxPolygonVertices = 8;
  protected
    m_polygonA, m_polygonB: b2PolygonShapeWrapper;
    m_transformA, m_transformB: b2Transform;
    m_positionB: b2Vec2;
    m_angleB: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Dynamics, DebugDraw;

{ TPolyCollision }

constructor TPolyCollision.Create;
begin
  inherited Create;

  m_polygonA := b2PolygonShapeWrapper.Create;
  m_polygonA.SetAsBox(0.2, 0.4);
  m_transformA.&Set(b2Vec2.Create(0.0, 0.0), 0.0);

  m_polygonB := b2PolygonShapeWrapper.Create;
  m_polygonB.SetAsBox(0.5, 0.5);
  m_positionB.&Set(19.345284, 1.5632932);
  m_angleB := 1.9160721;
  m_transformB.&Set(m_positionB, m_angleB);
end;

class function TPolyCollision.CreateTest: TTest;
begin
  Result := TPolyCollision.Create;
end;

procedure TPolyCollision.Keyboard(key: Integer);
begin
  case key of
    Ord('A'), Ord('a'):
      m_positionB.x := m_positionB.x - 0.1;

    Ord('D'), Ord('d'):
      m_positionB.x := m_positionB.x + 0.1;

    Ord('S'), Ord('s'):
      m_positionB.y := m_positionB.y - 0.1;

    Ord('W'), Ord('w'):
      m_positionB.y := m_positionB.y + 0.1;

    Ord('Q'), Ord('q'):
      m_angleB := m_angleB + 0.1 * pi;

    Ord('E'), Ord('e'):
      m_angleB := m_angleB - 0.1 * pi;
  end;

  m_transformB.&Set(m_positionB, m_angleB);
end;

procedure TPolyCollision.Step(settings: PSettings);
var
  manifold: b2Manifold;
  color: b2Color;
  worldManifold: b2WorldManifold;
  v: array [0..b2_maxPolygonVertices-1] of b2Vec2;
  I: Integer;
begin
  //B2_NOT_USED(settings);

  manifold := b2Manifold.Create;
  b2CollidePolygons(@manifold, m_polygonA, m_transformA, m_polygonB, m_transformB);

  worldManifold := b2WorldManifold.Create;
  worldManifold.Initialize(@manifold, m_transformA, m_polygonA.m_radius, m_transformB, m_polygonB.m_radius);

  g_debugDraw.DrawString(5, m_textLine, 'point count = %d', [manifold.pointCount]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  color := b2Color.Create(0.9, 0.9, 0.9, 1.0);

  for I := 0 to m_polygonA.m_count - 1 do
  begin
    v[I] := b2Mul(m_transformA, m_polygonA.m_vertices[i]);
  end;

  g_debugDraw.DrawPolygon(@v[0], m_polygonA.m_count, color);

  for I := 0 to m_polygonB.m_count - 1 do
  begin
    v[i] := b2Mul(m_transformB, m_polygonB.m_vertices[i]);
  end;

  g_debugDraw.DrawPolygon(@v[0], m_polygonB.m_count, color);


  for I := 0 to manifold.pointCount - 1 do
    g_debugDraw.DrawPoint(worldManifold.points[i], 4.0, b2Color.Create(0.9, 0.3, 0.3, 1.0));

end;

initialization
  RegisterTest(TestEntry.Create('PolyCollision', @TPolyCollision.CreateTest));
end.

