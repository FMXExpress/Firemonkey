//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Gears;

interface

uses
  Box2D.Dynamics, Test;

type
  TGears = class(TTest)
  protected
    m_joint1, m_joint2: b2RevoluteJointWrapper;
    m_joint3: b2PrismaticJointWrapper;
    m_joint4, m_joint5: b2GearJointWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

constructor TGears.Create;
var
  ground, body1, body2, body3: b2BodyWrapper;
  bd, bd1, bd2, bd3: b2BodyDef;
  edgeShape: b2EdgeShapeWrapper;
  box: b2PolygonShapeWrapper;
  circle1, circle2: b2CircleShapeWrapper;
  jd1, jd2: b2RevoluteJointDef;
  joint1, joint2: b2JointWrapper;
  jd3: b2PrismaticJointDef;
  jd4, jd5: b2GearJointDef;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(50.0, 0.0), b2Vec2.Create(-50.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  //
  circle1 := b2CircleShapeWrapper.Create;
  circle1.m_radius := 1.0;

  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(0.5, 5.0);

  circle2 := b2CircleShapeWrapper.Create;
  circle2.m_radius := 2.0;

  bd1 := b2BodyDef.Create;
  bd1.&type := b2_staticBody;
  bd1.position.&Set(10.0, 9.0);
  body1 := m_world.CreateBody( @bd1);
  body1.CreateFixture(circle1, 5.0);

  bd2 := b2BodyDef.Create;
  bd2.&type := b2_dynamicBody;
  bd2.position.&Set(10.0, 8.0);
  body2 := m_world.CreateBody( @bd2);
  body2.CreateFixture(box, 5.0);

  bd3 := b2BodyDef.Create;
  bd3.&type := b2_dynamicBody;
  bd3.position.&Set(10.0, 6.0);
  body3 := m_world.CreateBody( @bd3);
  body3.CreateFixture(circle2, 5.0);

  jd1 := b2RevoluteJointDef.Create;
  jd1.Initialize(body2, body1, bd1.position);
  joint1 := m_world.CreateJoint( @jd1);

  jd2 := b2RevoluteJointDef.Create;
  jd2.Initialize(body2, body3, bd3.position);
  joint2 := m_world.CreateJoint( @jd2);

  jd4 := b2GearJointDef.Create;
  jd4.bodyA := body1;
  jd4.bodyB := body3;
  jd4.joint1 := joint1;
  jd4.joint2 := joint2;
  jd4.ratio := circle2.m_radius / circle1.m_radius;
  m_world.CreateJoint(@jd4);

  circle1.Destroy;
  circle2.Destroy;
  box.Destroy;

  //
  circle1 := b2CircleShapeWrapper.Create;
  circle1.m_radius := 1.0;

  circle2 := b2CircleShapeWrapper.Create;
  circle2.m_radius := 2.0;

  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(0.5, 5.0);

  bd1 := b2BodyDef.Create;
  bd1.&type := b2_dynamicBody;
  bd1.position.&Set(-3.0, 12.0);
  body1 := m_world.CreateBody(@bd1);
  body1.CreateFixture(circle1, 5.0);

  jd1 := b2RevoluteJointDef.Create;
  jd1.bodyA := ground;
  jd1.bodyB := body1;
  jd1.localAnchorA := ground.GetLocalPoint(bd1.position);
  jd1.localAnchorB := body1.GetLocalPoint(bd1.position);
  jd1.referenceAngle := body1.GetAngle() - ground.GetAngle();
  m_joint1 := m_world.CreateJoint(@jd1);

  bd2 := b2BodyDef.Create;
  bd2.&type := b2_dynamicBody;
  bd2.position.&Set(0.0, 12.0);
  body2 := m_world.CreateBody(@bd2);
  body2.CreateFixture(circle2, 5.0);

  jd2 := b2RevoluteJointDef.Create;
  jd2.Initialize(ground, body2, bd2.position);
  m_joint2 := m_world.CreateJoint( @jd2);

  bd3 := b2BodyDef.Create;
  bd3.&type := b2_dynamicBody;
  bd3.position.&Set(2.5, 12.0);
  body3 := m_world.CreateBody( @bd3);
  body3.CreateFixture(box, 5.0);

  jd3 := b2PrismaticJointDef.Create;
  jd3.Initialize(ground, body3, bd3.position, b2Vec2.Create(0.0, 1.0));
  jd3.lowerTranslation := -5.0;
  jd3.upperTranslation := 5.0;
  jd3.enableLimit := true;

  m_joint3 := m_world.CreateJoint(@jd3);

  jd4 := b2GearJointDef.Create;
  jd4.bodyA := body1;
  jd4.bodyB := body2;
  jd4.joint1 := m_joint1;
  jd4.joint2 := m_joint2;
  jd4.ratio := circle2.m_radius / circle1.m_radius;
  m_joint4 := m_world.CreateJoint( @jd4);

  jd5 := b2GearJointDef.Create;
  jd5.bodyA := body2;
  jd5.bodyB := body3;
  jd5.joint1 := m_joint2;
  jd5.joint2 := m_joint3;
  jd5.ratio := -1.0 / circle2.m_radius;
  m_joint5 := m_world.CreateJoint( @jd5);

  circle1.Destroy;
  circle2.Destroy;
  box.Destroy;
end;

class function TGears.CreateTest: TTest;
begin
  Result := TGears.Create;
end;

procedure TGears.Step(settings: PSettings);
var
  ratio, value: float32;
begin
  inherited Step(settings);

  ratio := m_joint4.GetRatio;
  value := m_joint1.GetJointAngle + ratio * m_joint2.GetJointAngle;
  g_debugDraw.DrawString(5, m_textLine, 'theta1 + %4.2f * theta2 = %4.2f', [ratio, value]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  ratio := m_joint5.GetRatio;
  value := m_joint2.GetJointAngle + ratio * m_joint3.GetJointTranslation;
  g_debugDraw.DrawString(5, m_textLine, 'theta2 + %4.2f * delta = %4.2f', [ratio, value]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Gears', @TGears.CreateTest));
end.

