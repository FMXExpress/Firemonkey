//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit SliderCrank;

interface

uses
  Box2D.Dynamics, Test;

type
  TSliderCrank = class(TTest)
  protected
    m_joint1: b2RevoluteJointWrapper;
    m_joint2: b2PrismaticJointWrapper;
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

{ TSliderCrank }

constructor TSliderCrank.Create;
var
  ground, prevBody, body: b2BodyWrapper;
  bd: b2BodyDef;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  rjd: b2RevoluteJointDef;
  pjd: b2PrismaticJointDef;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  prevBody := ground;

  // Define crank.
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 2.0);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 7.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 2.0);
  polyShape.Destroy;

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(prevBody, body, b2Vec2.Create(0.0, 5.0));
  rjd.motorSpeed := 1.0 * pi;
  rjd.maxMotorTorque := 10000.0;
  rjd.enableMotor := True;
  m_joint1 := m_world.CreateJoint(@rjd);

  prevBody := body;

  // Define follower.
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 4.0);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 13.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 2.0);
  polyShape.Destroy;

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(prevBody, body, b2Vec2.Create(0.0, 9.0));
  rjd.enableMotor := False;
  m_world.CreateJoint(@rjd);

  prevBody := body;

  // Define piston
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(1.5, 1.5);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.fixedRotation := true;
  bd.position.&Set(0.0, 17.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 2.0);
  polyShape.Destroy;

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(prevBody, body, b2Vec2.Create(0.0, 17.0));
  m_world.CreateJoint(@rjd);

  pjd := b2PrismaticJointDef.Create;
  pjd.Initialize(ground, body, b2Vec2.Create(0.0, 17.0), b2Vec2.Create(0.0, 1.0));

  pjd.maxMotorForce := 1000.0;
  pjd.enableMotor := True;

  m_joint2 := m_world.CreateJoint(@pjd);

  // Create a payload
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(1.5, 1.5);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 23.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 2.0);
  polyShape.Destroy;
end;

class function TSliderCrank.CreateTest: TTest;
begin
  Result := TSliderCrank.Create;
end;

procedure TSliderCrank.Keyboard(key: Integer);
var
  body: b2BodyWrapper;
begin
  case key of
    Ord('F'), Ord('f'):
    begin
			m_joint2.EnableMotor(not m_joint2.IsMotorEnabled);
      body := m_joint2.GetBodyB;
			body.SetAwake(True);
    end;

    Ord('M'), Ord('m'):
    begin
			m_joint1.EnableMotor(not m_joint1.IsMotorEnabled);
      body := m_joint1.GetBodyB;
			body.SetAwake(True);
    end;
  end;
end;

procedure TSliderCrank.Step(settings: PSettings);
var
  torque: Float32;
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Keys: (f) toggle friction, (m) toggle motor');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  torque := m_joint1.GetMotorTorque(settings.hz);
  g_debugDraw.DrawString(5, m_textLine, 'Motor Torque = %5.0f', [torque]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

end;

initialization
  RegisterTest(TestEntry.Create('SliderCrank', @TSliderCrank.CreateTest));
end.
