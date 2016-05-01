//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit VaryingRestitution;

interface

uses Test;

type
  TVaryingRestitution = class(TTest)
  public
    constructor Create;
    class function CreateTest: TTest; static;
  end;

implementation

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TVaryingRestitution }

constructor TVaryingRestitution.Create;
const
  restitution: array[0..6] of float32 = (0.0, 0.1, 0.3, 0.5, 0.75, 0.9, 1.0);
var
  ground, body: b2BodyWrapper;
  bd: b2BodyDef;
  shape: b2EdgeShapeWrapper;
  circleShape: b2CircleShapeWrapper;
  fd: b2FixtureDef;
  i: Integer;
begin
  inherited;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  shape := b2EdgeShapeWrapper.Create;
  shape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(shape, 0.0);

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 1.0;

  fd := b2FixtureDef.Create;
  fd.shape := circleShape;
  fd.density := 1.0;

  for i := Low(restitution) to High(restitution) do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-10.0 + 3.0 * i, 20.0);
    body := m_world.CreateBody(@bd);

    fd.restitution := restitution[i];
    body.CreateFixture(@fd);
  end;

  shape.Destroy;
  circleShape.Destroy;
end;

class function TVaryingRestitution.CreateTest: TTest;
begin
  Result := TVaryingRestitution.Create;
end;

initialization
  RegisterTest(TestEntry.Create('VaryingRestitution', @TVaryingRestitution.CreateTest));

end.