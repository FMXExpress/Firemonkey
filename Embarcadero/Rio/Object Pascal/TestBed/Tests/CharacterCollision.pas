//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit CharacterCollision;

interface

uses
  Box2D.Dynamics, Test;

type
  TCharacterCollision = class(TTest)
  protected
    m_character: b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;


constructor TCharacterCollision.Create;
var
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  vs: array [0..9] of b2Vec2;
  chainShape: b2ChainShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  circle: b2CircleShapeWrapper;
  fd: b2FixtureDef;

  angle: float32;
  delta: float32;
  vertices: array [0..5] of b2Vec2;
  I: Integer;
begin
  inherited Create;
  // Ground body
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  // Collinear edges with no adjacency information.
  // This shows the problematic case where a box shape can hit
  // an internal vertex.
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-8.0, 1.0), b2Vec2.Create(-6.0, 1.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.&Set(b2Vec2.Create(-6.0, 1.0), b2Vec2.Create(-4.0, 1.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.&Set(b2Vec2.Create(-4.0, 1.0), b2Vec2.Create(-2.0, 1.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  // Chain shape
  bd := b2BodyDef.Create;
  bd.angle := 0.25 * pi;
  ground := m_world.CreateBody(@bd);

  vs[0].&Set(5.0, 7.0);
  vs[1].&Set(6.0, 8.0);
  vs[2].&Set(7.0, 8.0);
  vs[3].&Set(8.0, 7.0);
  chainShape :=  b2ChainShapeWrapper.Create;
  chainShape.CreateChain(@vs[0], 4);
  ground.CreateFixture(chainShape, 0.0);
  chainShape.Destroy;

  // Square tiles. This shows that adjacency shapes may
  // have non-smooth collision. There is no solution
  // to this problem.
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(1.0, 1.0, b2Vec2.Create(4.0, 3.0), 0.0);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.SetAsBox(1.0, 1.0, b2Vec2.Create(6.0, 3.0), 0.0);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.SetAsBox(1.0, 1.0, b2Vec2.Create(8.0, 3.0), 0.0);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;

  // Square made from an edge loop. Collision should be smooth.
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  vs[0].&Set(-1.0, 3.0);
  vs[1].&Set(1.0, 3.0);
  vs[2].&Set(1.0, 5.0);
  vs[3].&Set(-1.0, 5.0);
  chainShape := b2ChainShapeWrapper.Create;
  chainShape.CreateLoop(@vs[0], 4);
  ground.CreateFixture(chainShape, 0.0);
  chainShape.Destroy;

  // Edge loop. Collision should be smooth.
  bd := b2BodyDef.Create;
  bd.position.&Set(-10.0, 4.0);
  ground := m_world.CreateBody(@bd);

  vs[0].&Set(0.0, 0.0);
  vs[1].&Set(6.0, 0.0);
  vs[2].&Set(6.0, 2.0);
  vs[3].&Set(4.0, 1.0);
  vs[4].&Set(2.0, 2.0);
  vs[5].&Set(0.0, 2.0);
  vs[6].&Set(-2.0, 2.0);
  vs[7].&Set(-4.0, 3.0);
  vs[8].&Set(-6.0, 2.0);
  vs[9].&Set(-6.0, 0.0);
  chainShape := b2ChainShapeWrapper.Create;
  chainShape.CreateLoop(@vs[0], 10);
  ground.CreateFixture(chainShape, 0.0);
  chainShape.Destroy;

  // Square character 1
  bd := b2BodyDef.Create;
  bd.position.&Set(-3.0, 8.0);
  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.allowSleep := false;

  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 0.5);

  FD := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  body.CreateFixture(@fd);
  polyShape.Destroy;

  // Square character 2
  bd := b2BodyDef.Create;
  bd.position.&Set(-5.0, 5.0);
  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.allowSleep := false;

  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.25, 0.25);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  body.CreateFixture(@fd);
  polyShape.Destroy;

  // Hexagon character
  bd := b2BodyDef.Create;
  bd.position.&Set(-5.0, 8.0);
  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.allowSleep := false;

  body := m_world.CreateBody(@bd);

  angle := 0.0;
  delta := pi / 3.0;
  for I := 0 to 5 do
  begin
    vertices[I].&Set(0.5 * cos(angle), 0.5 * sin(angle));
    angle := angle + delta;
  end;

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.&Set(@vertices[0], 6);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  body.CreateFixture(@fd);
  polyShape.Destroy;

  // Circle character
  bd := b2BodyDef.Create;
  bd.position.&Set(3.0, 5.0);
  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.allowSleep := false;

  body := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.5;

  fd := b2FixtureDef.Create;
  fd.shape := circle;
  fd.density := 20.0;
  body.CreateFixture(@fd);
  circle.Destroy;

  // Circle character
  bd := b2BodyDef.Create;
  bd.position.&Set(-7.0, 6.0);
  bd.&type := b2_dynamicBody;
  bd.allowSleep := false;

  m_character := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.25;

  fd := b2FixtureDef.Create;
  fd.shape := circle;
  fd.density := 20.0;
  fd.friction := 1.0;
  m_character.CreateFixture(@fd);
  circle.Destroy;
end;

class function TCharacterCollision.CreateTest: TTest;
begin
  Result := TCharacterCollision.Create;
end;

procedure TCharacterCollision.Step(settings: PSettings);
var
  v: b2Vec2;
begin
  v := m_character.GetLinearVelocity^;
  v.x := -5.0;
  m_character.SetLinearVelocity(v);

  inherited Step(settings);

  g_debugDraw.DrawString(5, m_textLine, 'This tests various character collision shapes.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'Limitation: square and hexagon can snag on aligned boxes.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'Feature: edge chains have smooth collision inside and out.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('CharacterCollision', @TCharacterCollision.CreateTest));
end.

