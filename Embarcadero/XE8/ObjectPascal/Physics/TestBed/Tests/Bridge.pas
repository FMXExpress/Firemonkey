
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Bridge;

interface

uses
  Box2D.Dynamics, Test;

type
  TBridge = class(TTest)
  protected
    m_middle: b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;
  end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;


constructor TBridge.Create;
const
  e_count = 30;
var
  I: Integer;
  ground: b2BodyWrapper;
  bd: b2BodyDef;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  jd: b2RevoluteJointDef;
  prevBody: b2BodyWrapper;
  body: b2BodyWrapper;
  anchor: b2Vec2;
  vertices: array [0..2] of b2Vec2;
  circShape: b2CircleShapeWrapper;
begin
(*
  b2Body* ground = NULL;
  {
    b2BodyDef bd;
    ground = m_world->CreateBody(&bd);

    b2EdgeShape shape;
    shape.Set(b2Vec2(-40.0f, 0.0f), b2Vec2(40.0f, 0.0f));
    ground->CreateFixture(&shape, 0.0f);
  }

  {
    b2PolygonShape shape;
    shape.SetAsBox(0.5f, 0.125f);

    b2FixtureDef fd;
    fd.shape = &shape;
    fd.density = 20.0f;
    fd.friction = 0.2f;

    b2RevoluteJointDef jd;

    b2Body* prevBody = ground;
    for (int32 i = 0; i < e_count; ++i)
    {
      b2BodyDef bd;
      bd.type = b2_dynamicBody;
      bd.position.Set(-14.5f + 1.0f * i, 5.0f);
      b2Body* body = m_world->CreateBody(&bd);
      body->CreateFixture(&fd);

      b2Vec2 anchor(-15.0f + 1.0f * i, 5.0f);
      jd.Initialize(prevBody, body, anchor);
      m_world->CreateJoint(&jd);

      if (i == (e_count >> 1))
      {
        m_middle = body;
      }
      prevBody = body;
    }

    b2Vec2 anchor(-15.0f + 1.0f * e_count, 5.0f);
    jd.Initialize(prevBody, ground, anchor);
    m_world->CreateJoint(&jd);
  }

  for (int32 i = 0; i < 2; ++i)
  {
    b2Vec2 vertices[3];
    vertices[0].Set(-0.5f, 0.0f);
    vertices[1].Set(0.5f, 0.0f);
    vertices[2].Set(0.0f, 1.5f);

    b2PolygonShape shape;
    shape.Set(vertices, 3);

    b2FixtureDef fd;
    fd.shape = &shape;
    fd.density = 1.0f;

    b2BodyDef bd;
    bd.type = b2_dynamicBody;
    bd.position.Set(-8.0f + 8.0f * i, 12.0f);
    b2Body* body = m_world->CreateBody(&bd);
    body->CreateFixture(&fd);
  }

  for (int32 i = 0; i < 3; ++i)
  {
    b2CircleShape shape;
    shape.m_radius = 0.5f;

    b2FixtureDef fd;
    fd.shape = &shape;
    fd.density = 1.0f;

    b2BodyDef bd;
    bd.type = b2_dynamicBody;
    bd.position.Set(-6.0f + 6.0f * i, 10.0f);
    b2Body* body = m_world->CreateBody(&bd);
    body->CreateFixture(&fd);
  }
*)
  inherited Create;
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  fd.friction := 0.2;

  jd := b2RevoluteJointDef.Create;

  prevBody := ground;
  for I := 0 to e_count - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-14.5 + 1.0 * I, 5.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(-15.0 + 1.0 * I, 5.0);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    if i = (e_count shr 1) then
      m_middle := body;

    prevBody := body;
  end;

  anchor := b2Vec2.Create(-15.0 + 1.0 * e_count, 5.0);
  jd.Initialize(prevBody, ground, anchor);
  m_world.CreateJoint(@jd);
  polyShape.Destroy;

  for I := 0 to 1 do
  begin
    vertices[0].&Set(-0.5, 0.0);
    vertices[1].&Set(0.5, 0.0);
    vertices[2].&Set(0.0, 1.5);

    polyShape := b2PolygonShapeWrapper.Create;
    polyShape.&Set(@vertices[0], 3);

    fd := b2FixtureDef.Create;
    fd.shape := polyShape;
    fd.density := 1.0;

    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-8.0 + 8.0 * I, 12.0);

    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);
    polyShape.Destroy;
  end;

  for I := 0 to 2 do
  begin
    circShape := b2CircleShapeWrapper.Create;
    circShape.Set_m_radius(0.5);

    fd := b2FixtureDef.Create;
    fd.shape := circShape;
    fd.density := 1.0;

    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-6.0 + 6.0 * i, 10.0);

    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);
    circShape.Destroy;
  end;
end;

class function TBridge.CreateTest: TTest;
begin
  Result := TBridge.Create;
end;

initialization
  RegisterTest(TestEntry.Create('Bridge', @TBridge.CreateTest));

end.

