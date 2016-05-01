//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ApplyForce;

interface

uses
  Test, Box2D.Dynamics;

type
  TApplyForce = class(TTest)
  protected
    m_body: b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
  end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TApplyForce }

constructor TApplyForce.Create;
const
  k_restitution = 0.4;
  gravity = 10.0;
var
  I: Integer;
  ground, body: b2BodyWrapper;
  bd: b2BodyDef;
  shape: b2EdgeShapeWrapper;
  shape2: b2PolygonShapeWrapper;
  sd, sd1, sd2: b2FixtureDef;
  fd: b2FixtureDef;
  xf1, xf2: b2Transform;
  vertices: array [0..2] of b2Vec2;
  poly1, poly2: b2PolygonShapeWrapper;
  inertia, mass, radius: Float32;
  jd: b2FrictionJointDef;

begin
  inherited;

  m_world.SetGravity(b2Vec2.Create(0.0, 0.0));

  bd := b2BodyDef.Create;
  bd.position.&Set(0.0, 20.0);
  ground := m_world.CreateBody(@bd);

  shape := b2EdgeShapeWrapper.Create;

  sd := b2FixtureDef.Create;
  sd.shape := shape;
  sd.density := 0.0;
  sd.restitution := k_restitution;

  // Left vertical
  shape.&Set(b2Vec2.Create(-20.0, -20.0), b2Vec2.Create(-20.0, 20.0));
  ground.CreateFixture(@sd);

  // Right vertical
  shape.&Set(b2Vec2.Create(20.0, -20.0), b2Vec2.Create(20.0, 20.0));
  ground.CreateFixture(@sd);

  // Top horizontal
  shape.&Set(b2Vec2.Create(-20.0, 20.0), b2Vec2.Create(20.0, 20.0));
  ground.CreateFixture(@sd);

  // Bottom horizontal
  shape.&Set(b2Vec2.Create(-20.0, -20.0), b2Vec2.Create(20.0, -20.0));
  ground.CreateFixture(@sd);


  xf1 := b2Transform.Create;
  xf1.q.&Set(0.3524 * Pi);
  xf1.p := xf1.q.GetXAxis;

  vertices[0] := b2Mul(xf1, b2Vec2.Create(-1.0, 0.0));
  vertices[1] := b2Mul(xf1, b2Vec2.Create(1.0, 0.0));
  vertices[2] := b2Mul(xf1, b2Vec2.Create(0.0, 0.5));

  poly1 := b2PolygonShapeWrapper.Create;
  poly1.&Set(@vertices[0], 3);

  sd1 := b2FixtureDef.Create;
  sd1.shape := poly1;
  sd1.density := 4.0;

  xf2 := b2Transform.Create;
  xf2.q.&Set(-0.3524 * Pi);
  xf2.p := -xf2.q.GetXAxis;

  vertices[0] := b2Mul(xf2, b2Vec2.Create(-1.0, 0.0));
  vertices[1] := b2Mul(xf2, b2Vec2.Create(1.0, 0.0));
  vertices[2] := b2Mul(xf2, b2Vec2.Create(0.0, 0.5));

  poly2 := b2PolygonShapeWrapper.Create;
  poly2.&Set(@vertices[0], 3);

  sd2 := b2FixtureDef.Create;
  sd2.shape := poly2;
  sd2.density := 2.0;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.angularDamping := 2.0;
  bd.linearDamping := 0.5;

  bd.position.&Set(0.0, 2.0);
  bd.angle := pi;
  bd.allowSleep := false;
  m_body := m_world.CreateBody(@bd);
  m_body.CreateFixture(@sd1);
  m_body.CreateFixture(@sd2);

  shape2 := b2PolygonShapeWrapper.Create;
  shape2.SetAsBox(0.5, 0.5);

  fd := b2FixtureDef.Create;
  fd.shape := shape2;
  fd.density := 1.0;
  fd.friction := 0.3;

  for I := 0 to 10 - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;

    bd.position.&Set(0.0, 5.0 + 1.54 * I);
    body := m_world.CreateBody(@bd);

    body.CreateFixture(@fd);

    inertia := body.GetInertia;
    mass := body.GetMass;

    // For a circle: I = 0.5 * m * r * r ==> r = sqrt(2 * I / m)
    radius := sqrt(2.0 * I / mass);

    jd := b2FrictionJointDef.Create;
    jd.localAnchorA.SetZero;
    jd.localAnchorB.SetZero;
    jd.bodyA := ground;
    jd.bodyB := body;
    jd.collideConnected := true;
    jd.maxForce := mass * gravity;
    jd.maxTorque := mass * radius * gravity;

    m_world.CreateJoint(@jd);
  end;

  shape.Destroy;
  poly1.Destroy;
  poly2.Destroy;
  shape2.Destroy;
end;

class function TApplyForce.CreateTest: TTest;
begin
  Result := TApplyForce.Create;
end;

procedure TApplyForce.Keyboard(key: Integer);
var
  f, p: b2Vec2;
begin
  case key of
		Ord('W'), Ord('w'):
    begin
				f := m_body.GetWorldVector(b2Vec2.Create(0.0, -200.0));
				p := m_body.GetWorldPoint(b2Vec2.Create(0.0, 2.0));
				m_body.ApplyForce(f, p, true);
    end;

		Ord('A'), Ord('a'):
				m_body.ApplyTorque(50.0, true);

		Ord('D'), Ord('d'):
				m_body.ApplyTorque(-50.0, true);

  end;
end;

initialization
  RegisterTest(TestEntry.Create('ApplyForce', @TApplyForce.CreateTest));

end.
