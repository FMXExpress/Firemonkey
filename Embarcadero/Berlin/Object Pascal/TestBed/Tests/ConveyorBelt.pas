//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ConveyorBelt;

interface

uses
  Box2D.Dynamics, Box2D.Collision, Test;

type
  TConveyorBelt = class(TTest)
  protected
      m_platform: pb2Fixture;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold); override; cdecl;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, DebugDraw;


constructor TConveyorBelt.Create;
var
  I: Integer;
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  body: b2BodyWrapper;
  shape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;

begin
  inherited Create;
  // Ground
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));
  ground.CreateFixture(edge, 0.0);

  // Platform
  bd := b2BodyDef.Create;
  bd.position.&Set(-5.0, 5.0);
  body := m_world.CreateBody(@bd);

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(10.0, 0.5);

  fd := b2FixtureDef.Create;
  fd.shape := shape;
  fd.friction := 0.8;
  m_platform := body.CreateFixture(@fd);
  shape.Destroy;

  // Boxes
  for I := 0 to 4 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-10.0 + 2.0 * I, 7.0);
    body := m_world.CreateBody(@bd);

    shape := b2PolygonShapeWrapper.Create;
    shape.SetAsBox(0.5, 0.5);
    body.CreateFixture(shape, 20.0);
    shape.Destroy;
  end;
end;

class function TConveyorBelt.CreateTest: TTest;
begin
  Result := TConveyorBelt.Create;
end;

procedure TConveyorBelt.PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold);
var
  LContact: b2ContactWrapper;
  fixtureA, fixtureB: pb2fixture;
begin
  inherited PreSolve(contact, oldManifold);

  LContact := contact;

  fixtureA := LContact.GetFixtureA;
  fixtureB := LContact.GetFixtureB;

  if (fixtureA = m_platform) then
    LContact.SetTangentSpeed(5.0);

  if (fixtureB = m_platform) then
    LContact.SetTangentSpeed(-5.0);

end;

procedure TConveyorBelt.Step(settings: PSettings);
begin
  inherited Step(settings);
end;

initialization
  RegisterTest(TestEntry.Create('ConveyorBelt', @TConveyorBelt.CreateTest));
end.

