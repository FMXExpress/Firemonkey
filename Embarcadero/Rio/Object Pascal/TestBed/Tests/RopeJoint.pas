//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit RopeJoint;

interface

uses
  Box2D.Dynamics, Test;

type
  TRopeJoint = class(TTest)
  protected
    m_ropeDef: b2RopeJointDef;
    m_rope: b2JointWrapper;
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

{ TRopeJoint }

constructor TRopeJoint.Create;
const
  N = 40;
  Y = 15.0;
var
  I: Integer;
  bd: b2BodyDef;
  fd: b2FixtureDef;
  ground, body, prevBody: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  jd: b2RevoluteJointDef;
  extraLength: Float32;
  anchor: b2Vec2;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  //
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 0.125);

  fd := b2FixtureDef.Create;
  fd.shape := polyShape;
  fd.density := 20.0;
  fd.friction := 0.2;
  fd.filter.categoryBits := $0001;
  fd.filter.maskBits := $FFFF and not $0002;

  jd := b2RevoluteJointDef.Create;
  jd.collideConnected := false;

  m_ropeDef := b2RopeJointDef.Create;
  m_ropeDef.localAnchorA.&Set(0.0, Y);

  prevBody := ground;
  for I := 0 to N - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(0.5 + 1.0 * I, Y);
    if (i = N - 1) then
    begin
      polyShape.SetAsBox(1.5, 1.5);
      fd.density := 100.0;
      fd.filter.categoryBits := $0002;
      bd.position.&Set(1.0 * i, y);
      bd.angularDamping := 0.4;
    end;

    body := m_world.CreateBody(@bd);

    body.CreateFixture(@fd);

    anchor := b2Vec2.Create(i, y);
    jd.Initialize(prevBody, body, anchor);
    m_world.CreateJoint(@jd);

    prevBody := body;
  end;
  polyShape.Destroy;

  m_ropeDef.localAnchorB.SetZero();

  extraLength := 0.01;
  m_ropeDef.maxLength := N - 1.0 + extraLength;
  m_ropeDef.bodyB := prevBody;

  //
  m_ropeDef.bodyA := ground;
  m_rope := m_world.CreateJoint(@m_ropeDef);

end;

class function TRopeJoint.CreateTest: TTest;
begin
  Result := TRopeJoint.Create;
end;

procedure TRopeJoint.Keyboard(key: Integer);
begin
  case key of

    Ord('J'), Ord('j'):
    begin
      if m_rope.FHandle <> 0 then
      begin
        m_world.DestroyJoint(m_rope);
        m_rope.FHandle := 0;
      end
      else
        m_rope := m_world.CreateJoint(@m_ropeDef);
    end;
  end;
end;

procedure TRopeJoint.Step(settings: PSettings);
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Press (j) to toggle the rope joint.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  if m_rope.FHandle <> 0 then
    g_debugDraw.DrawString(5, m_textLine, 'Rope ON')
  else
    g_debugDraw.DrawString(5, m_textLine, 'Rope OFF');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('RopeJoint', @TRopeJoint.CreateTest));
end.

