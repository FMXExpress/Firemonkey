//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit EdgeShapes;

interface

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, Test, DebugDraw;

type
  EdgeShapesCallback = class(TNoRefCount, Ib2RayCastCallback)
  private
    FHandle: b2QueryCallbackHandle;
  protected
    m_fixture: pb2Fixture;
    m_point, m_normal: b2Vec2;
  public
    constructor Create;
    destructor Destroy; override;
    property Handle: b2QueryCallbackHandle read FHandle;
    property Fixture: pb2Fixture read m_fixture;

    function ReportFixture(fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single;  cdecl;
  end;


  TEdgeShapes = class(TTest)
  protected
  const
		e_maxBodies = 256;
  protected
    m_bodyIndex: Integer;
    m_bodies: array [0..e_maxBodies-1] of b2BodyWrapper;
    m_polygons: array [0..3] of b2PolygonShapeWrapper;
    m_circle: b2CircleShapeWrapper;
    m_angle: Float32;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateTest: TTest; static;

    procedure CreateBody(index: Integer);
    procedure DestroyBody;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math;

{ TEdgeShapes }

constructor TEdgeShapes.Create;
var
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  x1, x2, y1, y2: Float32;
  w, b, s: Float32;
  I: Integer;
  edgeShape: b2EdgeShapeWrapper;
  vertices: array [0..7] of b2Vec2;
begin
  inherited Create;

  for I := 0 to 3 do
    m_polygons[I] := b2PolygonShapeWrapper.Create;

  m_circle := b2CircleShapeWrapper.Create;

  // Ground body
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  x1 := -20.0;
  y1 := 2.0 * cos(x1 / 10.0 * pi);
  for I := 0 to 79 do
  begin
    x2 := x1 + 0.5;
    y2 := 2.0 * cos(x2 / 10.0 * pi);

    edgeShape := b2EdgeShapeWrapper.Create;
    edgeShape.&Set(b2Vec2.Create(x1, y1), b2Vec2.Create(x2, y2));
    ground.CreateFixture(edgeShape, 0.0);

    x1 := x2;
    y1 := y2;
  end;

  //
  vertices[0].&Set(-0.5, 0.0);
  vertices[1].&Set(0.5, 0.0);
  vertices[2].&Set(0.0, 1.5);
  m_polygons[0].&Set(@vertices[0], 3);

  //
  vertices[0].&Set(-0.1, 0.0);
  vertices[1].&Set(0.1, 0.0);
  vertices[2].&Set(0.0, 1.5);
  m_polygons[1].&Set(@vertices[0], 3);


  //
  w := 1.0;
  b := w / (2.0 + Sqrt(2.0));
  s := Sqrt(2.0) * b;

  vertices[0].&Set(0.5 * s, 0.0);
  vertices[1].&Set(0.5 * w, b);
  vertices[2].&Set(0.5 * w, b + s);
  vertices[3].&Set(0.5 * s, w);
  vertices[4].&Set(-0.5 * s, w);
  vertices[5].&Set(-0.5 * w, b + s);
  vertices[6].&Set(-0.5 * w, b);
  vertices[7].&Set(-0.5 * s, 0.0);

  m_polygons[2].&Set(@vertices[0], 8);

  //
  m_polygons[3].SetAsBox(0.5, 0.5);

  m_circle.m_radius := 0.5;

  m_bodyIndex := 0;

  //memset(m_bodies, 0, sizeof(m_bodies));
  for I := Low(m_bodies) to High(m_bodies) do
    m_bodies[I].FHandle := 0;


  m_angle := 0.0;
end;

procedure TEdgeShapes.CreateBody(index: Integer);
var
  bd: b2BodyDef;
  x, y: Float32;
  fd: b2FixtureDef;
begin

  if (m_bodies[m_bodyIndex].FHandle <> 0) then
  begin
    m_world.DestroyBody(m_bodies[m_bodyIndex]);
    m_bodies[m_bodyIndex].FHandle := 0;
  end;

  bd := b2BodyDef.Create;

  x := RandomFloat(-10.0, 10.0);
  y := RandomFloat(10.0, 20.0);
  bd.position.&Set(x, y);
  bd.angle := RandomFloat(-pi, pi);
  bd.&type := b2_dynamicBody;

  if (index = 4) then
    bd.angularDamping := 0.02;


  m_bodies[m_bodyIndex] := m_world.CreateBody(@bd);

  if (index < 4) then
  begin
    fd := b2FixtureDef.Create;
    fd.shape := m_polygons[index];
    fd.friction := 0.3;
    fd.density := 20.0;
    m_bodies[m_bodyIndex].CreateFixture(@fd);
  end
  else
  begin
    fd := b2FixtureDef.Create;
    fd.shape := m_circle;
    fd.friction := 0.3;
    fd.density := 20.0;
    m_bodies[m_bodyIndex].CreateFixture(@fd);
  end;

  m_bodyIndex := (m_bodyIndex + 1) mod e_maxBodies;

end;

class function TEdgeShapes.CreateTest: TTest;
begin
  Result := TEdgeShapes.Create;
end;

destructor TEdgeShapes.Destroy;
var
  I: Integer;
begin
  m_circle.Destroy;

  for I := 0 to 3 do
    m_polygons[I].Destroy;

  inherited;
end;

procedure TEdgeShapes.DestroyBody;
var
  I: Integer;
begin
  for I := 0 to e_maxBodies - 1 do
  begin
    if (m_bodies[i].FHandle <> 0) then
    begin
      m_world.DestroyBody(m_bodies[i]);
      m_bodies[i].FHandle := 0;
      Break;
    end
  end;
end;

procedure TEdgeShapes.Keyboard(key: Integer);
begin
  case key of
    Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'):
			CreateBody(Ord(key) - Ord('1'));

    Ord('D'), Ord('d'):
			DestroyBody();
  end;
end;

procedure TEdgeShapes.Step(settings: PSettings);
var
  advanceRay: Boolean;
  L: Float32;
  point1, point2, d, head: b2Vec2;
  callback: EdgeShapesCallback;
begin
  inherited Step(settings);
  advanceRay := (settings.pause = False) or settings.singleStep;


  g_debugDraw.DrawString(5, m_textLine, 'Press 1-5 to drop stuff');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  L := 25.0;
  point1 := b2Vec2.Create(0.0, 10.0);
  d := b2Vec2.Create(L * cos(m_angle), -L * Abs(sin(m_angle)));
  point2 := point1 + d;


  callback := EdgeShapesCallback.Create;

  m_world.RayCast(callback.Handle, point1, point2);

  if (callback.Fixture <> nil) then
  begin
    g_debugDraw.DrawPoint(callback.m_point, 5.0, b2Color.Create(0.4, 0.9, 0.4, 1.0));

    g_debugDraw.DrawSegment(point1, callback.m_point, b2Color.Create(0.8, 0.8, 0.8, 1.0));

    head := callback.m_point + 0.5 * callback.m_normal;
    g_debugDraw.DrawSegment(callback.m_point, head, b2Color.Create(0.9, 0.9, 0.4, 1.0));
  end
  else
    g_debugDraw.DrawSegment(point1, point2, b2Color.Create(0.8, 0.8, 0.8, 1.0));

  if advanceRay then
    m_angle := m_angle + 0.25 * pi / 180.0;

end;

{ EdgeShapeCallback }

constructor EdgeShapesCallback.Create;
begin
  inherited Create;
  FHandle := Create_b2RayCastCallback_delegate(Self);
end;

destructor EdgeShapesCallback.Destroy;
begin
  Destroy_b2RayCastCallback_delegate(FHandle);
  inherited;
end;

function EdgeShapesCallback.ReportFixture(fixture: Pb2Fixture; const [ref] point: b2Vec2; const [ref] normal: b2Vec2; fraction: Single): Single;

begin
  m_fixture := fixture;
  m_point := point;
  m_normal := normal;

  Result := fraction;
end;

initialization
  RegisterTest(TestEntry.Create('EdgeShapes', @TEdgeShapes.CreateTest));
end.
