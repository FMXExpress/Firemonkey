//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Cantilever;

interface

uses
  Box2D.Dynamics, Test;

type
  TCantilever = class(TTest)
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


constructor TCantilever.Create;
const
  e_count = 8;
var
  ground: b2BodyWrapper;
  bd: b2BodyDef;
  edge: b2EdgeShapeWrapper;
  poly: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  jd: b2WeldJointDef;
  prevBody, body: b2BodyWrapper;
  I: Integer;
  anchor: b2Vec2;
  vertices: array [0..2] of b2Vec2;
  circle: b2CircleShapeWrapper;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edge, 0.0);


  poly := b2PolygonShapeWrapper.Create;
  poly.SetAsBox(0.5, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := poly;
  fd.density := 20.0;

  jd := b2WeldJointDef.Create;

  prevBody := ground;
  for I := 0 to e_count -1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-14.5 + 1.0 * i, 5.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(-15.0 + 1.0 * i, 5.0);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    prevBody := body;
  end;
  poly.Destroy;

  poly := b2PolygonShapeWrapper.Create;
  poly.SetAsBox(1.0, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := poly;
  fd.density := 20.0;

  jd := b2WeldJointDef.Create;
  jd.frequencyHz := 5.0;
  jd.dampingRatio := 0.7;

  prevBody := ground;
  for I := 0 to 2 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-14.0 + 2.0 * i, 15.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(-15.0 + 2.0 * i, 15.0);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    prevBody := body;
  end;
  poly.Destroy;

  poly := b2PolygonShapeWrapper.Create;
  poly.SetAsBox(0.5, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := poly;
  fd.density := 20.0;

  jd := b2WeldJointDef.Create;

  prevBody := ground;
  for I := 0 to e_count - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-4.5 + 1.0 * i, 5.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    if (i > 0) then
    begin
      anchor := b2Vec2.Create(-5.0 + 1.0 * i, 5.0);
      jd.Initialize(prevBody, body, anchor);
      m_world.CreateJoint(@jd);
    end;

    prevBody := body;
  end;
  poly.Destroy;

  poly := b2PolygonShapeWrapper.Create;
  poly.SetAsBox(0.5, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := poly;
  fd.density := 20.0;

  jd := b2WeldJointDef.Create;
  jd.frequencyHz := 8.0;
  jd.dampingRatio := 0.7;

  prevBody := ground;
  for I := 0 to e_count - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(5.5 + 1.0 * i, 10.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    if (i > 0) then
    begin
      anchor := b2Vec2.Create(5.0 + 1.0 * i, 10.0);
      jd.Initialize(prevBody, body, anchor);
      m_world.CreateJoint(@jd);
    end;

    prevBody := body;
  end;
  poly.Destroy;

  for I := 0 to 1 do
  begin
    vertices[0].&Set(-0.5, 0.0);
    vertices[1].&Set(0.5, 0.0);
    vertices[2].&Set(0.0, 1.5);

    poly := b2PolygonShapeWrapper.Create;
    poly.&Set(@vertices[0], 3);

    fd := b2FixtureDef.Create;
    fd.shape := poly;
    fd.density := 1.0;

    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-8.0 + 8.0 * I, 12.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    poly.Destroy;
  end;

  for I := 0 to 1 do
  begin
    circle := b2CircleShapeWrapper.Create;
    circle.m_radius := 0.5;

    fd := b2FixtureDef.Create;
    fd.shape := circle;
    fd.density := 1.0;

    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-6.0 + 6.0 * I, 10.0);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    circle.Destroy;
  end;
end;

class function TCantilever.CreateTest: TTest;
begin
  Result := TCantilever.Create;
end;

initialization
  RegisterTest(TestEntry.Create('Cantilever', @TCantilever.CreateTest));

end.

