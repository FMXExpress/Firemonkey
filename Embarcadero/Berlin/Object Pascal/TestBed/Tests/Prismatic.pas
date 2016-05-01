//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Prismatic;

interface

uses
  Box2D.Dynamics, Test;

type
  TPrismatic = class(TTest)
  protected
    m_joint: b2PrismaticJointWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TPrismatic }

constructor TPrismatic.Create;
var
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  bd: b2BodyDef;
  polyShape: b2PolygonShapeWrapper;
  axis: b2Vec2;
  pjd: b2PrismaticJointDef;
begin
  inherited Create;
  // Ground
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  // Body
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(2.0, 0.5);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-10.0, 10.0);
  bd.angle := 0.5 * pi;
  bd.allowSleep := false;
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 5.0);

  pjd := b2PrismaticJointDef.Create;
  polyShape.Destroy;

  // Bouncy limit
  axis := b2Vec2.Create(2.0, 1.0);
  axis.Normalize();
  pjd.Initialize(ground, body, b2Vec2.Create(0.0, 0.0), axis);

  // Non-bouncy limit
  //pjd.Initialize(ground, body, b2Vec2.Create(-10.0, 10.0), b2Vec2.Create(1.0, 0.0));

  pjd.motorSpeed := 10.0;
  pjd.maxMotorForce := 10000.0;
  pjd.enableMotor := true;
  pjd.lowerTranslation := 0.0;
  pjd.upperTranslation := 20.0;
  pjd.enableLimit := true;

  m_joint := m_world.CreateJoint(@pjd);
end;

class function TPrismatic.CreateTest: TTest;
begin
  Result := TPrismatic.Create;
end;

procedure TPrismatic.Keyboard(key: Integer);
begin
  case key of
    Ord('L'), Ord('l'):
			m_joint.EnableLimit(not m_joint.IsLimitEnabled);
    Ord('M'), Ord('m'):
			m_joint.EnableMotor(not m_joint.IsMotorEnabled);
    Ord('S'), Ord('s'):
			m_joint.SetMotorSpeed(-m_joint.GetMotorSpeed);
  end;
end;

procedure TPrismatic.Step(settings: PSettings);
var
 force: Float32;
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Keys: (l) limits, (m) motors, (s) speed');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  force := m_joint.GetMotorForce(settings.hz);
  g_debugDraw.DrawString(5, m_textLine, 'Motor Force = %4.0f', [force]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Prismatic', @TPrismatic.CreateTest));
end.

