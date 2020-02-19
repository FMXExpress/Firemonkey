//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TheoJansen;

interface

uses
  Box2D.Common, Box2D.Dynamics, Test;

type
  TTheoJansen = class(TTest)
  protected
    m_offset: b2Vec2;
    m_chassis: b2BodyWrapper;
    m_wheel: b2BodyWrapper;
    m_motorJoint: b2RevoluteJointWrapper;
    m_motorOn: Boolean;
    m_motorSpeed: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure CreateLeg(s: float32; const [Ref] wheelAnchor: b2Vec2);

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Collision, DebugDraw;

{ TTheoJansen }

constructor TTheoJansen.Create;
const
  numElements = 40;
var
  bd: b2BodyDef;
  pivot: b2Vec2;
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  circleShape: b2CircleShapeWrapper;
  I: Integer;
  polyShape: b2PolygonShapeWrapper;
  sd: b2FixtureDef;
  wheelAnchor: b2Vec2;
  jd: b2RevoluteJointDef;

begin
  inherited Create;

  m_offset.&Set(0.0, 8.0);
  m_motorSpeed := 2.0;
  m_motorOn := true;
  pivot := b2Vec2.Create(0.0, 0.8);

  // Ground
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-50.0, 0.0), b2Vec2.Create(50.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);

  edgeShape.&Set(b2Vec2.Create(-50.0, 0.0), b2Vec2.Create(-50.0, 10.0));
  ground.CreateFixture(edgeShape, 0.0);

  edgeShape.&Set(b2Vec2.Create(50.0, 0.0), b2Vec2.Create(50.0, 10.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  // Balls
  for I := 0 to numElements - 1 do
  begin
    circleShape := b2CircleShapeWrapper.Create;
    circleShape.m_radius := 0.25;

    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-40.0 + 2.0 * I, 0.5);

    body := m_world.CreateBody(@bd);
    body.CreateFixture(circleShape, 1.0);
  end;
  circleShape.Destroy;

  // Chassis
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(2.5, 1.0);

  sd := b2FixtureDef.Create;
  sd.density := 1.0;
  sd.shape := polyShape;
  sd.filter.groupIndex := -1;
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position := pivot + m_offset;
  m_chassis := m_world.CreateBody(@bd);
  m_chassis.CreateFixture(@sd);
  polyShape.Destroy;

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 1.6;

  sd.density := 1.0;
  sd.shape := circleShape;
  sd.filter.groupIndex := -1;
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position := pivot + m_offset;
  m_wheel := m_world.CreateBody(@bd);
  m_wheel.CreateFixture(@sd);

  jd := b2RevoluteJointDef.Create;
  jd.Initialize(m_wheel, m_chassis, pivot + m_offset);
  jd.collideConnected := False;
  jd.motorSpeed := m_motorSpeed;
  jd.maxMotorTorque := 400.0;
  jd.enableMotor := m_motorOn;
  m_motorJoint := m_world.CreateJoint(@jd);


  wheelAnchor := pivot + b2Vec2.Create(0.0, -0.8);

  CreateLeg(-1.0, wheelAnchor);
  CreateLeg(1.0, wheelAnchor);

  m_wheel.SetTransform(m_wheel.GetPosition^, 120.0 * pi / 180.0);
  CreateLeg(-1.0, wheelAnchor);
  CreateLeg(1.0, wheelAnchor);

  m_wheel.SetTransform(m_wheel.GetPosition^, -120.0 * pi / 180.0);
  CreateLeg(-1.0, wheelAnchor);
  CreateLeg(1.0, wheelAnchor);

end;

procedure TTheoJansen.CreateLeg(s: float32; const [Ref] wheelAnchor: b2Vec2);
var
  p1, p2, p3, p4, p5, p6: b2Vec2;
  fd1, fd2: b2FixtureDef;
  poly1, poly2: b2PolygonShapeWrapper;
  vertices: array [0..2] of b2Vec2;
  bd1, bd2: b2BodyDef;
  body1, body2: b2BodyWrapper;
  djd: b2DistanceJointDef;
  rjd: b2RevoluteJointDef;

begin
  p1 := b2Vec2.Create(5.4 * s, -6.1);
  p2 := b2Vec2.Create(7.2 * s, -1.2);
  p3 := b2Vec2.Create(4.3 * s, -1.9);
  p4 := b2Vec2.Create(3.1 * s, 0.8);
  p5 := b2Vec2.Create(6.0 * s, 1.5);
  p6 := b2Vec2.Create(2.5 * s, 3.7);

  fd1.filter.groupIndex := -1;
  fd2.filter.groupIndex := -1;
  fd1.density := 1.0;
  fd2.density := 1.0;


  poly1 := b2PolygonShapeWrapper.Create;
  poly2 := b2PolygonShapeWrapper.Create;
  if (s > 0.0) then
  begin
    vertices[0] := p1;
    vertices[1] := p2;
    vertices[2] := p3;
    poly1.&Set(@vertices[0], 3);

    vertices[0] := b2Vec2.Create(0.0, 0.0);
    vertices[1] := p5 - p4;
    vertices[2] := p6 - p4;
    poly2.&Set(@vertices[0], 3);
  end
  else
  begin
    vertices[0] := p1;
    vertices[1] := p3;
    vertices[2] := p2;
    poly1.&Set(@vertices[0], 3);

    vertices[0] := b2Vec2.Create(0.0, 0.0);
    vertices[1] := p6 - p4;
    vertices[2] := p5 - p4;
    poly2.&Set(@vertices[0], 3);
  end;

  fd1.shape := poly1;
  fd2.shape := poly2;

  bd1 := b2BodyDef.Create;
  bd2 := b2BodyDef.Create;
  bd1.&type := b2_dynamicBody;
  bd2.&type := b2_dynamicBody;
  bd1.position := m_offset;
  bd2.position := p4 + m_offset;

  bd1.angularDamping := 10.0;
  bd2.angularDamping := 10.0;

  body1 := m_world.CreateBody(@bd1);
  body2 := m_world.CreateBody(@bd2);

  body1.CreateFixture(@fd1);
  body2.CreateFixture(@fd2);

  poly1.Destroy;
  poly2.Destroy;

  djd := b2DistanceJointDef.Create;

  // Using a soft distance constraint can reduce some jitter.
  // It also makes the structure seem a bit more fluid by
  // acting like a suspension system.
  djd.dampingRatio := 0.5;
  djd.frequencyHz := 10.0;

  djd.Initialize(body1, body2, p2 + m_offset, p5 + m_offset);
  m_world.CreateJoint(@djd);

  djd.Initialize(body1, body2, p3 + m_offset, p4 + m_offset);
  m_world.CreateJoint(@djd);

  djd.Initialize(body1, m_wheel, p3 + m_offset, wheelAnchor + m_offset);
  m_world.CreateJoint(@djd);

  djd.Initialize(body2, m_wheel, p6 + m_offset, wheelAnchor + m_offset);
  m_world.CreateJoint(@djd);

  rjd := b2RevoluteJointDef.Create;

  rjd.Initialize(body2, m_chassis, p4 + m_offset);
  m_world.CreateJoint(@rjd);
end;

class function TTheoJansen.CreateTest: TTest;
begin
  Result := TTheoJansen.Create;
end;

procedure TTheoJansen.Keyboard(key: Integer);
begin
  case key of
    Ord('A'), Ord('a'):
      m_motorJoint.SetMotorSpeed(-m_motorSpeed);

    Ord('S'), Ord('s'):
      m_motorJoint.SetMotorSpeed(0.0);

    Ord('D'), Ord('d'):
      m_motorJoint.SetMotorSpeed(m_motorSpeed);

    Ord('M'), Ord('m'):
      m_motorJoint.EnableMotor(not m_motorJoint.IsMotorEnabled);
  end;
end;

procedure TTheoJansen.Step(settings: PSettings);
begin
  g_debugDraw.DrawString(5, m_textLine, 'Keys: left = a, brake = s, right = d, toggle motor = m');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  inherited Step(settings);
end;

initialization
  RegisterTest(TestEntry.Create('TheoJansen', @TTheoJansen.CreateTest));
end.
