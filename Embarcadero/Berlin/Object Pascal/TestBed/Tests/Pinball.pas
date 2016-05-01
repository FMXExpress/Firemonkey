//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Pinball;

interface

uses
  Box2D.Dynamics, Test;

type
  TPinball = class(TTest)
  protected
    m_leftJoint, m_rightJoint: b2RevoluteJointWrapper;
    m_ball: b2BodyWrapper;
    m_button: Boolean;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure KeyboardUp(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TPinball }

constructor TPinball.Create;
var
  ground, leftFlipper, rightFlipper: b2BodyWrapper;
  loop: b2ChainShapeWrapper;
  bd: b2BodyDef;
  fd: b2FixtureDef;
  box: b2PolygonShapeWrapper;
  circleShape: b2CircleShapeWrapper;
  vs: array [0..4] of b2Vec2;
  p1, p2: b2Vec2;
  jd: b2RevoluteJointDef;
begin
  inherited Create;

  // Ground body
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  vs[0].&Set(0.0, -2.0);
  vs[1].&Set(8.0, 6.0);
  vs[2].&Set(8.0, 20.0);
  vs[3].&Set(-8.0, 20.0);
  vs[4].&Set(-8.0, 6.0);

  loop := b2ChainShapeWrapper.Create;
  loop.CreateLoop(@vs[0], 5);
  fd := b2FixtureDef.Create;
  fd.shape := loop;
  fd.density := 0.0;
  ground.CreateFixture(@fd);
  loop.Destroy;

  // Flippers
  p1 := b2Vec2.Create(-2.0, 0.0);
  p2 := b2Vec2.Create(2.0, 0.0);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;

  bd.position := p1;
  leftFlipper := m_world.CreateBody(@bd);

  bd.position := p2;
  rightFlipper := m_world.CreateBody(@bd);

  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(1.75, 0.1);

  fd := b2FixtureDef.Create;
  fd.shape := box;
  fd.density := 1.0;

  leftFlipper.CreateFixture(@fd);
  rightFlipper.CreateFixture(@fd);

  jd := b2RevoluteJointDef.Create;
  jd.bodyA := ground;
  jd.localAnchorB.SetZero();
  jd.enableMotor := true;
  jd.maxMotorTorque := 1000.0;
  jd.enableLimit := true;

  jd.motorSpeed := 0.0;
  jd.localAnchorA := p1;
  jd.bodyB := leftFlipper;
  jd.lowerAngle := -30.0 * pi / 180.0;
  jd.upperAngle := 5.0 * pi / 180.0;
  m_leftJoint := m_world.CreateJoint(@jd);

  jd.motorSpeed := 0.0;
  jd.localAnchorA := p2;
  jd.bodyB := rightFlipper;
  jd.lowerAngle := -5.0 * pi / 180.0;
  jd.upperAngle := 30.0 * pi / 180.0;
  m_rightJoint := m_world.CreateJoint(@jd);
  box.Destroy;

  // Circle character
  bd := b2BodyDef.Create;
  bd.position.&Set(1.0, 15.0);
  bd.&type := b2_dynamicBody;
  bd.bullet := True;

  m_ball := m_world.CreateBody(@bd);

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 0.2;

  fd := b2FixtureDef.Create;
  fd.shape := circleShape;
  fd.density := 1.0;
  m_ball.CreateFixture(@fd);
  circleShape.Destroy;

  m_button := false;
end;

class function TPinball.CreateTest: TTest;
begin
  Result := TPinball.Create;
end;

procedure TPinball.Keyboard(key: Integer);
begin
  case key of
    Ord('A'), Ord('a'):
			m_button := True;
  end;
end;

procedure TPinball.KeyboardUp(key: Integer);
begin
  case key of
    Ord('A'), Ord('a'):
			m_button := False;
  end;
end;

procedure TPinball.Step(settings: PSettings);
begin
  if (m_button) then
  begin
    m_leftJoint.SetMotorSpeed(20.0);
    m_rightJoint.SetMotorSpeed(-20.0);
  end
  else
  begin
    m_leftJoint.SetMotorSpeed(-10.0);
    m_rightJoint.SetMotorSpeed(10.0);
  end;
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Press ''a'' to control the flippers');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Pinball', @TPinball.CreateTest));
end.

