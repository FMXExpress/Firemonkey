//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit BasicSliderCrank;

interface

uses
  Test;

type

  TBasicSliderCrank = class(TTest)
  public
    constructor Create;
    class function CreateTest: TTest; static;
  end;

implementation

uses
  System.Math, Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;



{ TBasicSliderCrank }

constructor TBasicSliderCrank.Create;
var
  ground, body, prevBody: b2BodyWrapper;
  bd: b2BodyDef;
  shape: b2PolygonShapeWrapper;
  rjd: b2RevoluteJointDef;
  pjd: b2PrismaticJointDef;
begin
  inherited;
  bd := b2BodyDef.Create;
  bd.position.&Set(0.0, 17.0);
  ground := m_world.CreateBody(@bd);

  prevBody := ground;

  // Define crank.
  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(4.0, 1.0);

  bd.&type := b2_dynamicBody;
  bd.position.&Set(-8.0, 20.0);

  body := m_world.CreateBody(@bd);
  body.CreateFixture(shape, 2.0);

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(prevBody, body, b2Vec2.Create(-12.0, 20.0));
  m_world.CreateJoint(@rjd);

  prevBody := body;

  // Define connecting rod
  shape.SetAsBox(8.0, 1.0);

  bd.&type := b2_dynamicBody;
  bd.position.&Set(4.0, 20.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(shape, 2.0);

  rjd.Initialize(prevBody, body, b2Vec2.Create(-4.0, 20.0));
  m_world.CreateJoint(@rjd);

  prevBody := body;

  // Define piston
  shape.SetAsBox(3.0, 3.0);

  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.position.&Set(12.0, 20.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(shape, 2.0);

  rjd.Initialize(prevBody, body, b2Vec2.Create(12.0, 20.0));
  m_world.CreateJoint(@rjd);

  pjd := b2PrismaticJointDef.Create;
  pjd.Initialize(ground, body, b2Vec2.Create(12.0, 17.0), b2Vec2.Create(1.0, 0.0));
  m_world.CreateJoint(@pjd);

  shape.Destroy;
end;

class function TBasicSliderCrank.CreateTest: TTest;
begin
  Result := TBasicSliderCrank.Create;
end;

initialization
  RegisterTest(TestEntry.Create('BasicSliderCrank', @TBasicSliderCrank.CreateTest));

end.

