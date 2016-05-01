//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Dominos;

interface

uses
  Test;

type
  TDominos = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TDominos.Create;
var
  body, b1, b2, b3, b4, b5, b6, b7: b2BodyWrapper;
  bodyA, bodyB: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  polyShape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  I: Integer;
  jd: b2RevoluteJointDef;
  anchor, d: b2Vec2;
  djd: b2DistanceJointDef;
  radius: Float32;
  circleShape: b2CircleShapeWrapper;

begin
  inherited Create;

  // Body 1
  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));

  bd := b2BodyDef.Create;
  b1 := m_world.CreateBody(@bd);
  b1.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  //
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(6.0, 0.25);

  bd := b2BodyDef.Create;
  bd.position.&Set(-1.5, 10.0);
  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;

  //
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.1, 1.0);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  fd.friction := 0.1;

  for I := 0 to 9 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-6.0 + 1.0 * I, 11.25);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);
  end;
  polyShape.Destroy;

  //
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(7.0, 0.25, b2Vec2.Create(0.0,0.0), 0.3);

  bd := b2BodyDef.Create;
  bd.position.&Set(1.0, 6.0);
  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;

  // Body 2
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.25, 1.5);

  bd := b2BodyDef.Create;
  bd.position.&Set(-7.0, 4.0);
  b2 := m_world.CreateBody(@bd);
  b2.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;

  // Body 3
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(6.0, 0.125);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-0.9, 1.0);
  bd.angle := -0.15;

  b3 := m_world.CreateBody(@bd);
  b3.CreateFixture(polyShape, 10.0);

  jd := b2RevoluteJointDef.Create;

  anchor := b2Vec2.Create(-2.0, 1.0);
  jd.Initialize(b1, b3, anchor);
  jd.collideConnected := true;
  m_world.CreateJoint(@jd);
  polyShape.Destroy;

  //b2Body* b4;
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.25, 0.25);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-10.0, 15.0);
  b4 := m_world.CreateBody(@bd);
  b4.CreateFixture(polyShape, 10.0);

  anchor.&Set(-7.0, 15.0);
  jd := b2RevoluteJointDef.Create;
  jd.Initialize(b2, b4, anchor);
  m_world.CreateJoint(@jd);
  polyShape.Destroy;

  //b2Body* b5;
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(6.5, 3.0);
  b5 := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  fd := b2FixtureDef.Create;

  fd.shape := polyShape;
  fd.density := 10.0;
  fd.friction := 0.1;

  polyShape.SetAsBox(1.0, 0.1, b2Vec2.Create(0.0, -0.9), 0.0);
  b5.CreateFixture(@fd);

  polyShape.SetAsBox(0.1, 1.0, b2Vec2.Create(-0.9, 0.0), 0.0);
  b5.CreateFixture(@fd);

  polyShape.SetAsBox(0.1, 1.0, b2Vec2.Create(0.9, 0.0), 0.0);
  b5.CreateFixture(@fd);
  polyShape.Destroy;

  anchor.&Set(6.0, 2.0);
  jd := b2RevoluteJointDef.Create;
  jd.Initialize(b1, b5, anchor);
  m_world.CreateJoint(@jd);

  //b2Body* b6;
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(1.0, 0.1);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(6.5, 4.1);
  b6 := m_world.CreateBody(@bd);
  b6.CreateFixture(polyShape, 30.0);
  polyShape.Destroy;

  anchor.&Set(7.5, 4.0);
  jd := b2RevoluteJointDef.Create;
  jd.Initialize(b5, b6, anchor);
  m_world.CreateJoint(@jd);

  //b2Body* b7;
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.1, 1.0);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(7.4, 1.0);

  b7 := m_world.CreateBody(@bd);
  b7.CreateFixture(polyShape, 10.0);
  polyShape.Destroy;

  djd := b2DistanceJointDef.Create;
  djd.bodyA := b3;
  djd.bodyB := b7;
  djd.localAnchorA.&Set(6.0, 0.0);
  djd.localAnchorB.&Set(0.0, -1.0);
  bodyA := djd.bodyB;
  bodyB := djd.bodyB;
  d := bodyB.GetWorldPoint(djd.localAnchorB) - bodyA.GetWorldPoint(djd.localAnchorA);
  djd.length := d.Length;
  m_world.CreateJoint(@djd);

  radius := 0.2;

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := radius;

  for I := 0 to 3 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(5.9 + 2.0 * radius * I, 2.4);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(circleShape, 10.0);
  end;
  circleShape.Destroy;

end;

class function TDominos.CreateTest: TTest;
begin
  Result := TDominos.Create;
end;

initialization
  RegisterTest(TestEntry.Create('Dominos', @TDominos.CreateTest));
end.

