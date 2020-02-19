//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit AddPair;

interface

uses
  Test;

type
  TAddPair = class(TTest)
  public
    constructor Create;
    class function CreateTest: TTest; static;
  end;

implementation

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TAddPair }

constructor TAddPair.Create;
const
  minX = -6.0;
  maxX = 0.0;
  minY = 4.0;
  maxY = 6.0;

var
  I: Integer;
  shape: b2CircleShapeWrapper;
  shape2: b2PolygonShapeWrapper;
  bd: b2BodyDef;
  body: b2BodyWrapper;
begin
  inherited;
  m_world.SetGravity(b2Vec2.Create(0.0, 0.0));

  shape := b2CircleShapeWrapper.Create;
  shape.m_p.SetZero;
  shape.m_radius := 0.1;

  for I := 0 to 400 - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position := b2Vec2.Create(RandomFloat(minX,maxX),RandomFloat(minY,maxY));
    body := m_world.CreateBody(@bd);
    body.CreateFixture(shape, 0.01);
  end;

  shape2 := b2PolygonShapeWrapper.Create;
  shape2.SetAsBox(1.5, 1.5);
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-40.0, 5.0);
  bd.bullet := true;
  body := m_world.CreateBody(@bd);
  body.CreateFixture(shape2, 1.0);
  body.SetLinearVelocity(b2Vec2.Create(150.0, 0.0));

  shape.Destroy;
  shape2.Destroy;
end;

class function TAddPair.CreateTest: TTest;
begin
  Result := TAddPair.Create;
end;

initialization
  RegisterTest(TestEntry.Create('AddPair', @TAddPair.CreateTest));

end.
