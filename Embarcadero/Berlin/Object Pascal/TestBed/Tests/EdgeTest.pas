//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit EdgeTest;

interface

uses
  Test;

type
  TEdgeTest = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TEdgeTest.Create;
var
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  v1, v2, v3, v4, v5, v6, v7: b2Vec2;
  edge: b2EdgeShapeWrapper;
  circle: b2CircleShapeWrapper;
  shape: b2PolygonShapeWrapper;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  v1 := b2Vec2.Create(-10.0, 0.0);
  v2 := b2Vec2.Create(-7.0, -2.0);
  v3 := b2Vec2.Create(-4.0, 0.0);
  v4 := b2Vec2.Create(0.0, 0.0);
  v5 := b2Vec2.Create(4.0, 0.0);
  v6 := b2Vec2.Create(7.0, 2.0);
  v7 := b2Vec2.Create(10.0, 0.0);

  edge := b2EdgeShapeWrapper.Create;

  edge.&Set(v1, v2);
  edge.m_hasVertex3 := true;
  edge.m_vertex3 := v3;
  ground.CreateFixture(edge, 0.0);

  edge.&Set(v2, v3);
  edge.m_hasVertex0 := true;
  edge.m_hasVertex3 := true;
  edge.m_vertex0 := v1;
  edge.m_vertex3 := v4;
  ground.CreateFixture(edge, 0.0);

  edge.&Set(v3, v4);
  edge.m_hasVertex0 := true;
  edge.m_hasVertex3 := true;
  edge.m_vertex0 := v2;
  edge.m_vertex3 := v5;
  ground.CreateFixture(edge, 0.0);

  edge.&Set(v4, v5);
  edge.m_hasVertex0 := true;
  edge.m_hasVertex3 := true;
  edge.m_vertex0 := v3;
  edge.m_vertex3 := v6;
  ground.CreateFixture(edge, 0.0);

  edge.&Set(v5, v6);
  edge.m_hasVertex0 := true;
  edge.m_hasVertex3 := true;
  edge.m_vertex0 := v4;
  edge.m_vertex3 := v7;
  ground.CreateFixture(edge, 0.0);

  edge.&Set(v6, v7);
  edge.m_hasVertex0 := true;
  edge.m_vertex0 := v5;
  ground.CreateFixture(edge, 0.0);
  edge.Destroy;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-0.5, 0.6);
  bd.allowSleep := false;
  body := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.5;

  body.CreateFixture(circle, 1.0);
  circle.Destroy;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(1.0, 0.6);
  bd.allowSleep := false;
  body := m_world.CreateBody(@bd);

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(0.5, 0.5);

  body.CreateFixture(shape, 1.0);
  shape.Destroy;

end;

class function TEdgeTest.CreateTest: TTest;
begin
  Result := TEdgeTest.Create;
end;

initialization
  RegisterTest(TestEntry.Create('EdgeTest', @TEdgeTest.CreateTest));
end.

