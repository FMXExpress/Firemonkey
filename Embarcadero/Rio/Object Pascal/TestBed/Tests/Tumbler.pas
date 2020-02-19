//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Tumbler;

interface

uses
  Box2D.Dynamics, Test;

type
  TTumbler = class(TTest)
  protected
  const
		e_count = 800;
  protected
    m_joint: b2RevoluteJointWrapper;
    m_count: Int32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TTumbler }

constructor TTumbler.Create;
var
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  polyShape: b2PolygonShapeWrapper;
  jd: b2RevoluteJointDef;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.allowSleep := false;
  bd.position.&Set(0.0, 10.0);
  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 10.0, b2Vec2.Create( 10.0, 0.0), 0.0);
  body.CreateFixture(polyShape, 5.0);
  polyShape.SetAsBox(0.5, 10.0, b2Vec2.Create(-10.0, 0.0), 0.0);
  body.CreateFixture(polyShape, 5.0);
  polyShape.SetAsBox(10.0, 0.5, b2Vec2.Create(0.0, 10.0), 0.0);
  body.CreateFixture(polyShape, 5.0);
  polyShape.SetAsBox(10.0, 0.5, b2Vec2.Create(0.0, -10.0), 0.0);
  body.CreateFixture(polyShape, 5.0);
  polyShape.Destroy;

  jd := b2RevoluteJointDef.Create;
  jd.bodyA := ground;
  jd.bodyB := body;
  jd.localAnchorA.&Set(0.0, 10.0);
  jd.localAnchorB.&Set(0.0, 0.0);
  jd.referenceAngle := 0.0;
  jd.motorSpeed := 0.05 * pi;
  jd.maxMotorTorque := 1e8;
  jd.enableMotor := True;
  m_joint := m_world.CreateJoint(@jd);

  m_count := 0;
end;

class function TTumbler.CreateTest: TTest;
begin
  Result := TTumbler.Create;
end;

procedure TTumbler.Step(settings: PSettings);
var
  bd: b2BodyDef;
  body: b2BodyWrapper;
  polyShape: b2PolygonShapeWrapper;
begin
  inherited Step(settings);

  if (m_count < e_count) then
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(0.0, 10.0);
    body := m_world.CreateBody(@bd);

    polyShape := b2PolygonShapeWrapper.Create;
    polyShape.SetAsBox(0.125, 0.125);
    body.CreateFixture(polyShape, 1.0);
    polyShape.Destroy;

    Inc(m_count);
  end;

end;

initialization
  RegisterTest(TestEntry.Create('Tumbler', @TTumbler.CreateTest));
end.
