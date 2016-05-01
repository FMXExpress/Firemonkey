//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Revolute;

interface

uses
  Box2D.Dynamics, Test;

type
  TRevolute = class(TTest)
  protected
	  m_ball: b2BodyWrapper;
	  m_joint: b2RevoluteJointWrapper;
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

{ TRevolute }

constructor TRevolute.Create;
var
  bd: b2BodyDef;
  fd: b2FixtureDef;
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  circleShape: b2CircleShapeWrapper;
  rjd: b2RevoluteJointDef;
  w: Float32;
  verts: array[0..2] of b2Vec2;
begin
  inherited Create;

  //
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));

  fd := b2FixtureDef.Create;
  fd.shape := edgeShape;
  //fd.filter.categoryBits := 2;

  ground.CreateFixture(@fd);
  edgeShape.Destroy;

  //
  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 0.5;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;

  rjd := b2RevoluteJointDef.Create;

  bd.position.&Set(-10.0, 20.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(circleShape, 5.0);
  circleShape.Destroy;

  w := 100.0;
  body.SetAngularVelocity(w);
  body.SetLinearVelocity(b2Vec2.Create(-8.0 * w, 0.0));

  rjd.Initialize(ground, body, b2Vec2.Create(-10.0, 12.0));
  rjd.motorSpeed := 1.0 * pi;
  rjd.maxMotorTorque := 10000.0;
  rjd.enableMotor := false;
  rjd.lowerAngle := -0.25 * pi;
  rjd.upperAngle := 0.5 * pi;
  rjd.enableLimit := true;
  rjd.collideConnected := true;

  m_joint := m_world.CreateJoint(@rjd);

  //
  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 3.0;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(5.0, 30.0);

  fd := b2FixtureDef.Create;
  fd.density := 5.0;
  fd.filter.maskBits := 1;
  fd.shape := circleShape;

  m_ball := m_world.CreateBody(@bd);
  m_ball.CreateFixture(@fd);
  circleShape.Destroy;

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(10.0, 0.2, b2Vec2.Create(-10.0, 0.0), 0.0);

  bd := b2BodyDef.Create;
  bd.position.&Set(20.0, 10.0);
  bd.&type := b2_dynamicBody;
  bd.bullet := true;
  body := m_world.CreateBody(@bd);
  body.CreateFixture(polyShape, 2.0);

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(ground, body, b2Vec2.Create(20.0, 10.0));
  rjd.lowerAngle := -0.25 * pi;
  rjd.upperAngle := 0.0 * pi;
  rjd.enableLimit := true;
  m_world.CreateJoint(@rjd);

  // Tests mass computation of a small object far from the origin
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  verts[0].&Set(17.63, 36.31);
  verts[1].&Set(17.52, 36.69);
  verts[2].&Set(17.19, 36.36);
  polyShape.&Set(@verts[0], 3);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 1;

  body.CreateFixture(@fd);	//assertion hits inside here
end;

class function TRevolute.CreateTest: TTest;
begin
  Result := TRevolute.Create;
end;

procedure TRevolute.Keyboard(key: Integer);
begin
  inherited;
	case key of
    Ord('L'), Ord('l'):
			m_joint.EnableLimit(not m_joint.IsLimitEnabled);

    Ord('M'), Ord('m'):
			m_joint.EnableMotor(not m_joint.IsMotorEnabled);
  end;

end;

procedure TRevolute.Step(settings: PSettings);
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Keys: (l) limits, (m) motor');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Revolute', @TRevolute.CreateTest));
end.

