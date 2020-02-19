//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Web;

interface

uses
  Box2D.Dynamics, Test;

type
  TWeb = class(TTest)
  protected
    m_bodies: array [0..3] of b2BodyWrapper;
    m_joints: array [0..7] of b2JointWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure JointDestroyed(joint: b2JointHandle);

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TWeb }

constructor TWeb.Create;
var
  bd: b2BodyDef;
  ground, body, bA, bB: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  shape: b2PolygonShapeWrapper;
  jd: b2DistanceJointDef;
  p1, p2, d: b2Vec2;
  fd: b2FixtureDef;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edge, 0.0);
  edge.Destroy;

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(0.5, 0.5);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;

  bd.position.&Set(-5.0, 5.0);
  m_bodies[0] := m_world.CreateBody(@bd);
  m_bodies[0].CreateFixture(shape, 5.0);

  bd.position.&Set(5.0, 5.0);
  m_bodies[1] := m_world.CreateBody(@bd);
  m_bodies[1].CreateFixture(shape, 5.0);

  bd.position.&Set(5.0, 15.0);
  m_bodies[2] := m_world.CreateBody(@bd);
  m_bodies[2].CreateFixture(shape, 5.0);

  bd.position.&Set(-5.0, 15.0);
  m_bodies[3] := m_world.CreateBody(@bd);
  m_bodies[3].CreateFixture(shape, 5.0);

  jd := b2DistanceJointDef.Create;
  jd.frequencyHz := 2.0;
  jd.dampingRatio := 0.0;

  jd.bodyA := ground;
  jd.bodyB := m_bodies[0];
  jd.localAnchorA.&Set(-10.0, 0.0);
  jd.localAnchorB.&Set(-0.5, -0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[0] := m_world.CreateJoint(@jd);

  jd.bodyA := ground;
  jd.bodyB := m_bodies[1];
  jd.localAnchorA.&Set(10.0, 0.0);
  jd.localAnchorB.&Set(0.5, -0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[1] := m_world.CreateJoint(@jd);

  jd.bodyA := ground;
  jd.bodyB := m_bodies[2];
  jd.localAnchorA.&Set(10.0, 20.0);
  jd.localAnchorB.&Set(0.5, 0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[2] := m_world.CreateJoint(@jd);

  jd.bodyA := ground;
  jd.bodyB := m_bodies[3];
  jd.localAnchorA.&Set(-10.0, 20.0);
  jd.localAnchorB.&Set(-0.5, 0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[3] := m_world.CreateJoint(@jd);

  jd.bodyA := m_bodies[0];
  jd.bodyB := m_bodies[1];
  jd.localAnchorA.&Set(0.5, 0.0);
  jd.localAnchorB.&Set(-0.5, 0.0);;
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[4] := m_world.CreateJoint(@jd);

  jd.bodyA := m_bodies[1];
  jd.bodyB := m_bodies[2];
  jd.localAnchorA.&Set(0.0, 0.5);
  jd.localAnchorB.&Set(0.0, -0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[5] := m_world.CreateJoint(@jd);

  jd.bodyA := m_bodies[2];
  jd.bodyB := m_bodies[3];
  jd.localAnchorA.&Set(-0.5, 0.0);
  jd.localAnchorB.&Set(0.5, 0.0);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[6] := m_world.CreateJoint(@jd);

  jd.bodyA := m_bodies[3];
  jd.bodyB := m_bodies[0];
  jd.localAnchorA.&Set(0.0, -0.5);
  jd.localAnchorB.&Set(0.0, 0.5);
//  p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
//  p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
  bA := jd.bodyA;
  bB := jd.bodyB;
  p1 := bA.GetWorldPoint(jd.localAnchorA);
  p2 := bB.GetWorldPoint(jd.localAnchorB);
  d := p2 - p1;
  jd.length := d.Length();
  m_joints[7] := m_world.CreateJoint(@jd);


end;

class function TWeb.CreateTest: TTest;
begin
  Result := TWeb.Create;
end;

procedure TWeb.JointDestroyed(joint: b2JointHandle);
var
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    if m_joints[i].FHandle = joint then
    begin
      m_joints[i].FHandle := 0;
      Break;
    end;
  end;
end;

procedure TWeb.Keyboard(key: Integer);
var
  I: Integer;
begin
  inherited;
  case (key) of
    Ord('B'), Ord('b'):
			for I := 0 to 3 do
				if (m_bodies[i].FHandle <> 0) then
				begin
					m_world.DestroyBody(m_bodies[i]);
					m_bodies[i].FHandle := 0;
					Break;
				end;

    Ord('J'), Ord('j'):
			for I := 0 to 7 do
				if (m_joints[i].FHandle <> 0) then
				begin
					m_world.DestroyJoint(m_joints[i]);
					m_joints[i].FHandle := 0;
					Break;
				end;
  end;

end;

procedure TWeb.Step(settings: PSettings);
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'This demonstrates a soft distance joint.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'Press: (b) to delete a body, (j) to delete a joint');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Web', @TWeb.CreateTest));
end.
