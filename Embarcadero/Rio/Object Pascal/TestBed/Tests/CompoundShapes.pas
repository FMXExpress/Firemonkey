//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit CompoundShapes;

interface

uses
  Test;

type
  TCompoundShapes = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TCompoundShapes.Create;
var
  bd: b2BodyDef;
  body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  circle1: b2CircleShapeWrapper;
  circle2: b2CircleShapeWrapper;
  I: Integer;
  x: float32;
  polygon1: b2PolygonShapeWrapper;
  polygon2: b2PolygonShapeWrapper;
  xf1, xf2: b2Transform;
  vertices: array [0..2] of b2Vec2;
  triangle1: b2PolygonShapeWrapper;
  triangle2: b2PolygonShapeWrapper;

  bottom, left, right: b2PolygonShapeWrapper;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  bd.position.&Set(0.0, 0.0);
  body := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(50.0, 0.0), b2Vec2.Create(-50.0, 0.0));

  body.CreateFixture(edgeShape, 0.0);


  circle1 := b2CircleShapeWrapper.Create;
  circle1.m_radius := 0.5;
  circle1.m_p := b2Vec2.Create(-0.5, 0.5);

  circle2 := b2CircleShapeWrapper.Create;
  circle2.m_radius := 0.5;
  circle2.m_p := b2Vec2.Create(0.5, 0.5);

  for I := 0 to 9 do
  begin
    x := RandomFloat(-0.1, 0.1);
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(x + 5.0, 1.05 + 2.5 * I);
    bd.angle := RandomFloat(-pi, pi);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(circle1, 2.0);
    body.CreateFixture(circle2, 0.0);
  end;

  circle2.Destroy;
  circle1.Destroy;


  polygon1 := b2PolygonShapeWrapper.Create;
  polygon1.SetAsBox(0.25, 0.5);

  polygon2 := b2PolygonShapeWrapper.Create;
  polygon2.SetAsBox(0.25, 0.5, b2Vec2.Create(0.0, -0.5), 0.5 * pi);

  for I := 0 to 9 do
  begin
    x := RandomFloat(-0.1, 0.1);
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(x - 5.0, 1.05 + 2.5 * i);
    bd.angle := RandomFloat(-pi, pi);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(polygon1, 2.0);
    body.CreateFixture(polygon2, 2.0);
  end;

  polygon2.Destroy;
  polygon1.Destroy;


  xf1 := b2Transform.Create;
  xf1.q.&Set(0.3524 * pi);
  xf1.p := xf1.q.GetXAxis();


  triangle1 := b2PolygonShapeWrapper.Create;
  vertices[0] := b2Mul(xf1, b2Vec2.Create(-1.0, 0.0));
  vertices[1] := b2Mul(xf1, b2Vec2.Create(1.0, 0.0));
  vertices[2] := b2Mul(xf1, b2Vec2.Create(0.0, 0.5));
  triangle1.&Set(@vertices[0], 3);

  xf2 := b2Transform.Create;
  xf2.q.&Set(-0.3524 * pi);
  xf2.p := -xf2.q.GetXAxis();

  triangle2 := b2PolygonShapeWrapper.Create;
  vertices[0] := b2Mul(xf2, b2Vec2.Create(-1.0, 0.0));
  vertices[1] := b2Mul(xf2, b2Vec2.Create(1.0, 0.0));
  vertices[2] := b2Mul(xf2, b2Vec2.Create(0.0, 0.5));
  triangle2.&Set(@vertices[0], 3);

  for I := 0 to 9 do
  begin
    x := RandomFloat(-0.1, 0.1);
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(x, 2.05 + 2.5 * i);
    bd.angle := 0.0;
    body := m_world.CreateBody(@bd);
    body.CreateFixture(triangle1, 2.0);
    body.CreateFixture(triangle2, 2.0);
  end;
  triangle2.Destroy;
  triangle1.Destroy;


  bottom := b2PolygonShapeWrapper.Create;
  bottom.SetAsBox( 1.5, 0.15 );

  left := b2PolygonShapeWrapper.Create;
  left.SetAsBox(0.15, 2.7, b2Vec2.Create(-1.45, 2.35), 0.2);

  right := b2PolygonShapeWrapper.Create;
  right.SetAsBox(0.15, 2.7, b2Vec2.Create(1.45, 2.35), -0.2);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set( 0.0, 2.0 );
  body := m_world.CreateBody(@bd);
  body.CreateFixture(bottom, 4.0);
  body.CreateFixture(left, 4.0);
  body.CreateFixture(right, 4.0);

  right.Destroy;
  left.Destroy;
  bottom.Destroy;

end;

class function TCompoundShapes.CreateTest: TTest;
begin
  Result := TCompoundShapes.Create;
end;

initialization
  RegisterTest(TestEntry.Create('CompoundShapes', @TCompoundShapes.CreateTest));
end.

