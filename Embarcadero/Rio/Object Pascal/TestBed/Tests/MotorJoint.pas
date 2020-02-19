//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MotorJoint;

interface

uses
  Box2D.Dynamics, Test;

type
  TMotorJoint = class(TTest)
  protected
    m_joint: b2MotorJointWrapper;
    m_time: float32;
    m_go: Boolean;
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

{ TMotorJoint }

constructor TMotorJoint.Create;
var
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  bd: b2BodyDef;
  fd: b2FixtureDef;
  polyShape: b2PolygonShapeWrapper;
  mjd: b2MotorJointDef;
begin
  inherited Create;

  // Ground
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));

  fd := b2FixtureDef.Create;
  fd.shape := edgeShape;

  ground.CreateFixture(@fd);
  edgeShape.Destroy;

  // Define motorized body
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 8.0);
  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(2.0, 0.5);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.friction := 0.6;
  fd.density := 2.0;
  body.CreateFixture(@fd);

  mjd := b2MotorJointDef.Create;
  mjd.Initialize(ground, body);
  mjd.maxForce := 1000.0;
  mjd.maxTorque := 1000.0;
  m_joint := m_world.CreateJoint(@mjd);

  polyShape.Destroy;

  m_go := false;
  m_time := 0.0;
end;

class function TMotorJoint.CreateTest: TTest;
begin
  Result := TMotorJoint.Create;
end;

procedure TMotorJoint.Keyboard(key: Integer);
begin
  case key of
    Ord('S'), Ord('s'):
      m_go := not m_go;
  end;
end;

procedure TMotorJoint.Step(settings: PSettings);
var
  linearOffset: b2Vec2;
  angularOffset: Float32;
begin
  if m_go and (settings.hz > 0.0) then
  begin
    m_time := m_time + 1.0 / settings.hz;
  end;

  linearOffset.x := 6.0 * sin(2.0 * m_time);
  linearOffset.y := 8.0 + 4.0 * sin(1.0 * m_time);

  angularOffset := 4.0 * m_time;

  m_joint.SetLinearOffset(linearOffset);
  m_joint.SetAngularOffset(angularOffset);

  g_debugDraw.DrawPoint(linearOffset, 4.0, b2Color.Create(0.9, 0.9, 0.9, 1.0));

  inherited Step(settings);

  g_debugDraw.DrawString(5, m_textLine, 'Keys: (s) pause');
  m_textLine := m_textLine + 15;
end;

initialization
  RegisterTest(TestEntry.Create('MotorJoint', @TMotorJoint.CreateTest));
end.

