//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit VaryingFriction;

interface

uses
  Test;

type
  TVaryingFriction = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;

end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TVaryingFriction }

constructor TVaryingFriction.Create;
const
  friction: array[0..4] of Float32 = (0.75, 0.5, 0.35, 0.1, 0.0);
var
  I: Integer;
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(13.0, 0.25);

  bd := b2BodyDef.Create;
  bd.position.&Set(-4.0, 22.0);
  bd.angle := -0.25;

  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.25, 1.0);

  bd := b2BodyDef.Create;
  bd.position.&Set(10.5, 19.0);

  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(13.0, 0.25);

  bd := b2BodyDef.Create;
  bd.position.&Set(4.0, 14.0);
  bd.angle := 0.25;

  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.25, 1.0);

  bd := b2BodyDef.Create;
  bd.position.&Set(-10.5, 11.0);

  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(13.0, 0.25);

  bd := b2BodyDef.Create;
  bd.position.&Set(-4.0, 6.0);
  bd.angle := -0.25;

  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(polyShape, 0.0);
  polyShape.Destroy;


  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 0.5);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 25.0;


  for I := 0 to 4 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-15.0 + 4.0 * i, 28.0);
    body := m_world.CreateBody(@bd);

    fd.friction := friction[i];
    body.CreateFixture(@fd);
  end;
  polyShape.Destroy;

end;

class function TVaryingFriction.CreateTest: TTest;
begin
  Result := TVaryingFriction.Create;
end;

initialization
  RegisterTest(TestEntry.Create('VaryingFriction', @TVaryingFriction.CreateTest));
end.
