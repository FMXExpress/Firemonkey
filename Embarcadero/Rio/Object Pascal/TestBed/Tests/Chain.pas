//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Chain;

interface

uses Test, Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

type
  TChain = class(TTest)
  public
    constructor Create;
    class function CreateTest: TTest; static;
  end;

implementation

{ TChain }

constructor TChain.Create;
var
  ground, prevBody, body: b2BodyWrapper;
  bd: b2BodyDef;
  shape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  jd: b2RevoluteJointDef;
  y: float32;
  i: Integer;
  anchor: b2Vec2;
begin
  inherited;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  shape := b2EdgeShapeWrapper.Create;
  shape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(shape, 0.0);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.6, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  fd.friction := 0.2;

  jd := b2RevoluteJointDef.Create;
  jd.collideConnected := False;

  y := 25.0;
  prevBody := ground;
  for i := 0 to 29 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(0.5+i, y);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(i, y);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    prevBody := body;
  end;

  shape.Destroy;
  polyShape.Destroy;
end;

class function TChain.CreateTest: TTest;
begin
  Result := TChain.Create;
end;

initialization
  RegisterTest(TestEntry.Create('Chain', @TChain.CreateTest));

end.
