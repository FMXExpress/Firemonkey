//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit HeavyOnLight;

interface

uses
  Test;

type
  THeavyOnLight = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor THeavyOnLight.Create;
var
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  circle: b2CircleShapeWrapper;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge :=  b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edge, 0.0);
  edge.Destroy;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 0.5);
  body := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.5;
  body.CreateFixture(circle, 10.0);

  bd.position.&Set(0.0, 6.0);
  body := m_world.CreateBody(@bd);
  circle.m_radius := 5.0;
  body.CreateFixture(circle, 10.0);
  circle.Destroy;
end;

class function THeavyOnLight.CreateTest: TTest;
begin
  Result := THeavyOnLight.Create;
end;

initialization
  RegisterTest(TestEntry.Create('HeavyOnLight', @THeavyOnLight.CreateTest));
end.

