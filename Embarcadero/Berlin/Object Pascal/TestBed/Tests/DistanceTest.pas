//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit DistanceTest;

interface

uses
  Box2D.Collision, Box2D.Common, Test;

type
  TDistanceTest = class(TTest)
  private
  const
    b2_maxPolygonVertices = 8;
  protected
    m_positionB: b2Vec2;
    m_angleB: Float32;

    m_transformA: b2Transform;
    m_transformB: b2Transform;

    m_polygonA: b2PolygonShapeWrapper;
    m_polygonB: b2PolygonShapeWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
end;

implementation

uses
  System.Math,
  Box2D.Dynamics, DebugDraw;


constructor TDistanceTest.Create;
begin
  inherited Create;

  m_transformA.SetIdentity;
  m_transformA.p.&Set(0.0, -0.2);
  m_polygonA := b2PolygonShapeWrapper.Create;
  m_polygonA.SetAsBox(10.0, 0.2);

  m_positionB.&Set(12.017401, 0.13678508);
  m_angleB := -0.0109265;
  m_transformB.&Set(m_positionB, m_angleB);

  m_polygonB := b2PolygonShapeWrapper.Create;
  m_polygonB.SetAsBox(2.0, 0.1);
end;

class function TDistanceTest.CreateTest: TTest;
begin
  Result := TDistanceTest.Create;
end;

procedure TDistanceTest.Keyboard(key: Integer);
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

procedure TDistanceTest.Step(settings: PSettings);
var
  I: Integer;
  input: b2DistanceInput;
  output: b2DistanceOutput;
  color: b2Color;
  cache: b2SimplexCache;
  v: array [0..b2_maxPolygonVertices-1] of b2Vec2;
  x1, x2: b2Vec2;
begin
  inherited Step(settings);

  input := b2DistanceInput.Create;
  input.proxyA.&Set(m_polygonA, 0);
  input.proxyB.&Set(m_polygonB, 0);
  input.transformA := m_transformA;
  input.transformB := m_transformB;
  input.useRadii := true;
  cache := b2SimplexCache.Create;
  cache.count := 0;
  output := b2DistanceOutput.Create;
  Box2D.Collision.b2Distance(@output, @cache, @input);

  g_debugDraw.DrawString(5, m_textLine, 'Keys: left = a, down = s, right = d, up = w, cw = e, ccw = q');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  g_debugDraw.DrawString(5, m_textLine, 'distance = %g', [output.distance]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  g_debugDraw.DrawString(5, m_textLine, 'iterations = %d', [output.iterations]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  color := b2Color.Create(0.9, 0.9, 0.9, 1.0);

  for I := 0 to m_polygonA.m_count - 1 do
  begin
    v[i] := b2Mul(m_transformA, m_polygonA.m_vertices[i]);
  end;
  g_debugDraw.DrawPolygon(@v[0], m_polygonA.m_count, color);

  for I := 0 to m_polygonB.m_count - 1 do
  begin
    v[i] := b2Mul(m_transformB, m_polygonB.m_vertices[i]);
  end;
  g_debugDraw.DrawPolygon(@v[0], m_polygonB.m_count, color);


  x1 := output.pointA;
  x2 := output.pointB;

  g_debugDraw.DrawPoint(x1, 4.0, b2Color.Create(1.0, 0.0, 0.0, 1.0));
  g_debugDraw.DrawPoint(x2, 4.0, b2Color.Create(1.0, 1.0, 0.0, 1.0));
end;

initialization
  RegisterTest(TestEntry.Create('DistanceTest', @TDistanceTest.CreateTest));
end.

