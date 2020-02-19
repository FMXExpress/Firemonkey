//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Car;

interface

uses
  Box2D.Dynamics, Test;

type
  TCar = class(TTest)
  protected
    m_car: b2BodyWrapper;
    m_wheel1: b2BodyWrapper;
    m_wheel2: b2BodyWrapper;
    m_hz: Float32;
    m_zeta: Float32;
    m_speed: Float32;
    m_spring1: b2WheelJointWrapper;
    m_spring2: b2WheelJointWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;


constructor TCar.Create;
const
  hs: array [0..9] of Float32 = (0.25, 1.0, 4.0, 0.0, 0.0, -1.0, -2.0, -2.0, -1.25, 0.0);
  N = 20;

var
  I: Integer;
  ground: b2BodyWrapper;
  body, prevBody: b2BodyWrapper;
  bd: b2BodyDef;
  edgeShape: b2EdgeShapeWrapper;
  fd: b2FixtureDef;
  x, y1, y2, dx: Float32;
  polyShape: b2PolygonShapeWrapper;
  box: b2PolygonShapeWrapper;
  jd: b2RevoluteJointDef;
  anchor, axis: b2Vec2;
  chassis: b2PolygonShapeWrapper;
  vertices: array [0..7] of b2Vec2;
  circle: b2CircleShapeWrapper;
  jwd: b2WheelJointDef;
begin
  inherited Create;
  m_hz := 4.0;
  m_zeta := 0.7;

  m_speed := 50.0;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;

  fd := b2FixtureDef.Create;
  fd.shape := edgeShape;
  fd.density := 0.0;
  fd.friction := 0.6;

  edgeShape.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));
  ground.CreateFixture(@fd);

	x := 20.0;
  y1 := 0.0;
  dx := 5.0;

  for I := 0 to 9 do
  begin
    y2 := hs[I];
    edgeShape.&Set(b2Vec2.Create(x, y1), b2Vec2.Create(x + dx, y2));
    ground.CreateFixture(@fd);
    y1 := y2;
    x := x + dx;
  end;

  for I := 0 to 9 do
  begin
    y2 := hs[I];
    edgeshape.&Set(b2Vec2.Create(x, y1), b2Vec2.Create(x + dx, y2));
    ground.CreateFixture(@fd);
    y1 := y2;
    x := x + dx;
  end;

  edgeShape.&Set(b2Vec2.Create(x, 0.0), b2Vec2.Create(x + 40.0, 0.0));
  ground.CreateFixture(@fd);

  x := x + 80.0;
  edgeShape.&Set(b2Vec2.Create(x, 0.0), b2Vec2.Create(x + 40.0, 0.0));
  ground.CreateFixture(@fd);

  x := x + 40.0;
  edgeShape.&Set(b2Vec2.Create(x, 0.0), b2Vec2.Create(x + 10.0, 5.0));
  ground.CreateFixture(@fd);

  x := x + 20.0;
  edgeShape.&Set(b2Vec2.Create(x, 0.0), b2Vec2.Create(x + 40.0, 0.0));
  ground.CreateFixture(@fd);

  x := x + 40.0;
  edgeShape.&Set(b2Vec2.Create(x, 0.0), b2Vec2.Create(x, 20.0));
  ground.CreateFixture(@fd);

  edgeShape.Destroy;

  // Teeter
  bd := b2BodyDef.Create;
  bd.position.&Set(140.0, 1.0);
  bd.&type := b2_dynamicBody;
  body := m_world.CreateBody(@bd);

  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(10.0, 0.25);
  body.CreateFixture(box, 1.0);

  jd := b2RevoluteJointDef.Create;
  jd.Initialize(ground, body, body.GetPosition^);
  jd.lowerAngle := -8.0 * pi / 180.0;
  jd.upperAngle := 8.0 * pi / 180.0;
  jd.enableLimit := true;
  m_world.CreateJoint(@jd);

  body.ApplyAngularImpulse(100.0, true);

  box.Destroy;


// Bridge
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(1.0, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 1.0;
  fd.friction := 0.6;

  jd := b2RevoluteJointDef.Create;

  prevBody := ground;
  for I := 0 to N-1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(161.0 + 2.0 * I, -0.125);
    body := m_world.CreateBody(@bd);
    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(160.0 + 2.0 * I, -0.125);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    prevBody := body;
  end;

  anchor := b2Vec2.Create(160.0 + 2.0 * N, -0.125);
  jd.Initialize(prevBody, ground, anchor);
  m_world.CreateJoint(@jd);

  polyshape.Destroy;

// Boxes
  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(0.5, 0.5);

  body.FHandle := 0;
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;

  bd.position.&Set(230.0, 0.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(box, 0.5);

  bd.position.&Set(230.0, 1.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(box, 0.5);

  bd.position.&Set(230.0, 2.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(box, 0.5);

  bd.position.&Set(230.0, 3.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(box, 0.5);

  bd.position.&Set(230.0, 4.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(box, 0.5);


  box.Destroy;

  // Car
  chassis := b2PolygonShapeWrapper.Create;
  vertices[0].&Set(-1.5, -0.5);
  vertices[1].&Set(1.5, -0.5);
  vertices[2].&Set(1.5, 0.0);
  vertices[3].&Set(0.0, 0.9);
  vertices[4].&Set(-1.15, 0.9);
  vertices[5].&Set(-1.5, 0.2);
  chassis.&Set(@vertices[0], 6);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.4;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 1.0);
  m_car := m_world.CreateBody(@bd);
  m_car.CreateFixture(chassis, 1.0);

  chassis.Destroy;

  fd := b2FixtureDef.Create;
  fd.shape := circle;
  fd.density := 1.0;
  fd.friction := 0.9;

  circle.Destroy;

  bd.position.&Set(-1.0, 0.35);
  m_wheel1 := m_world.CreateBody(@bd);
  m_wheel1.CreateFixture(@fd);

  bd.position.&Set(1.0, 0.4);
  m_wheel2 := m_world.CreateBody(@bd);
  m_wheel2.CreateFixture(@fd);

  jwd := b2WheelJointDef.Create;
  axis := b2Vec2.Create(0.0, 1.0);

  jwd.Initialize(m_car, m_wheel1, m_wheel1.GetPosition^, axis);
  jwd.motorSpeed := 0.0;
  jwd.maxMotorTorque := 20.0;
  jwd.enableMotor := true;
  jwd.frequencyHz := m_hz;
  jwd.dampingRatio := m_zeta;
  m_spring1 := m_world.CreateJoint(@jwd);

  jwd.Initialize(m_car, m_wheel2, m_wheel2.GetPosition^, axis);
  jwd.motorSpeed := 0.0;
  jwd.maxMotorTorque := 10.0;
  jwd.enableMotor := false;
  jwd.frequencyHz := m_hz;
  jwd.dampingRatio := m_zeta;
  m_spring2 := m_world.CreateJoint(@jwd);
end;

class function TCar.CreateTest: TTest;
begin
  Result := TCar.Create;
end;

procedure TCar.Keyboard(key: Integer);
begin
  case key of
		Ord('A'), Ord('a'):
			m_spring1.SetMotorSpeed(m_speed);

		Ord('S'), Ord('s'):
			m_spring1.SetMotorSpeed(0.0);

		Ord('D'), Ord('d'):
			m_spring1.SetMotorSpeed(-m_speed);

		Ord('Q'), Ord('q'):
    begin
			m_hz := Max(0.0, m_hz - 1.0);
			m_spring1.SetSpringFrequencyHz(m_hz);
			m_spring2.SetSpringFrequencyHz(m_hz);
    end;

		Ord('E'), Ord('e'):
    begin
			m_hz := m_hz + 1.0;
			m_spring1.SetSpringFrequencyHz(m_hz);
			m_spring2.SetSpringFrequencyHz(m_hz);
    end;
  end;
end;

procedure TCar.Step(settings: PSettings);
begin
  g_debugDraw.DrawString(5, m_textLine, 'Keys: left = a, brake = s, right = d, hz down = q, hz up = e');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'frequency = %g hz, damping ratio = %g', [m_hz, m_zeta]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  g_camera.m_center.x := m_car.GetPosition.x;
  inherited Step(settings);
end;

initialization
  RegisterTest(TestEntry.Create('Car', @TCar.CreateTest));
end.

