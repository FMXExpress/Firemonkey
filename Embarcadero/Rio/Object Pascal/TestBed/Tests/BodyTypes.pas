//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit BodyTypes;

interface

uses
  Box2D.Dynamics, Test;

type
  TBodyTypes = class(TTest)
  protected
    m_attachment: b2BodyWrapper;
    m_platform: b2BodyWrapper;
    m_speed: Float32;
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


constructor TBodyTypes.Create;
var
  ground: b2BodyWrapper;
  bd: b2BodyDef;
  edgeshape: b2EdgeShapeWrapper;
  polygonshape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  rjd: b2RevoluteJointDef;
  pjd: b2PrismaticJointDef;
  body: b2BodyWrapper;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeshape := b2EdgeShapeWrapper.Create;
  edgeshape.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));

  fd := b2FixtureDef.Create;
  fd.shape := edgeshape;

  ground.CreateFixture(@fd);

  edgeshape.Destroy;

  // Define attachment
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 3.0);
  m_attachment := m_world.CreateBody(@bd);

  polygonshape := b2PolygonShapeWrapper.Create;
  polygonshape.SetAsBox(0.5, 2.0);
  m_attachment.CreateFixture(polygonshape, 2.0);
  polygonshape.Destroy;

  // Define platform
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-4.0, 5.0);
  m_platform := m_world.CreateBody(@bd);

  polygonshape := b2PolygonShapeWrapper.Create;
  polygonshape.SetAsBox(0.5, 4.0, b2Vec2.Create(4.0, 0.0), 0.5 * pi);

  fd := b2FixtureDef.Create;
  fd.shape := polygonshape;
  fd.friction := 0.6;
  fd.density := 2.0;
  m_platform.CreateFixture(@fd);

  rjd := b2RevoluteJointDef.Create;
  rjd.Initialize(m_attachment, m_platform, b2Vec2.Create(0.0, 5.0));
  rjd.maxMotorTorque := 50.0;
  rjd.enableMotor := true;
  m_world.CreateJoint(@rjd);

  pjd := b2PrismaticJointDef.Create;
  pjd.Initialize(ground, m_platform, b2Vec2.Create(0.0, 5.0), b2Vec2.Create(1.0, 0.0));

  pjd.maxMotorForce := 1000.0;
  pjd.enableMotor := true;
  pjd.lowerTranslation := -10.0;
  pjd.upperTranslation := 10.0;
  pjd.enableLimit := true;

  m_world.CreateJoint(@pjd);

  m_speed := 3.0;

  polygonshape.Destroy;

  // Create a payload
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 8.0);
  body := m_world.CreateBody(@bd);

  polygonshape := b2PolygonShapeWrapper.Create;
  polygonshape.SetAsBox(0.75, 0.75);

  fd := b2FixtureDef.Create;
  fd.shape := polygonshape;
  fd.friction := 0.6;
  fd.density := 2.0;
  body.CreateFixture(@fd);

  polygonshape.Destroy;
end;

class function TBodyTypes.CreateTest: TTest;
begin
  Result := TBodyTypes.Create;
end;

procedure TBodyTypes.Keyboard(key: Integer);
begin
  //inherited;
  case key of
    Ord('D'), Ord('d'):
      m_platform.SetType(b2_dynamicBody);

    Ord('S'), Ord('s'):
      m_platform.SetType(b2_staticBody);

    Ord('K'), Ord('k'):
    begin
      m_platform.SetType(b2_kinematicBody);
      m_platform.SetLinearVelocity(b2Vec2.Create(-m_speed, 0.0));
      m_platform.SetAngularVelocity(0.0);
    end;
  end;
end;

procedure TBodyTypes.Step(settings: PSettings);
var
  p, v: b2Vec2;
begin
  if m_platform.GetType = b2_kinematicBody then
  begin
    p := m_platform.GetTransform.p;
    v := m_platform.GetLinearVelocity^;

    if ((p.x < -10.0) and (v.x < 0.0)) or
       ((p.x > 10.0) and (v.x > 0.0)) then
    begin
      v.x := -v.x;
      m_platform.SetLinearVelocity(v);
    end;
  end;

  inherited Step(settings);

  g_debugDraw.DrawString(5, m_textLine, 'Keys: (d) dynamic, (s) static, (k) kinematic');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('BodyTypes', @TBodyTypes.CreateTest));
end.

