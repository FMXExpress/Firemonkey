//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Pulleys;

interface

uses
  Box2D.Dynamics, Test;

type
  TPulleys = class(TTest)
  protected
	  m_joint1: b2PulleyJointWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TPulleys }

constructor TPulleys.Create;
var
  y, L, a, b: Float32;
  ground, body1, body2: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  bd: b2BodyDef;
  circle: b2CircleShapeWrapper;
  shape: b2PolygonShapeWrapper;
  pulleyDef: b2PulleyJointDef;
  anchor1, anchor2, groundAnchor1, groundAnchor2: b2vec2;
begin
  inherited Create;

  y := 16.0;
  L := 12.0;
  a := 1.0;
  b := 2.0;

  //b2Body* ground = NULL;
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  //ground.CreateFixture(@shape, 0.0);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 2.0;

  circle.m_p := b2Vec2.Create(-10.0, y + b + L);
  ground.CreateFixture(circle, 0.0);

  circle.m_p := b2Vec2.Create(10.0, y + b + L);
  ground.CreateFixture(circle, 0.0);

  //
  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(a, b);

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;

  //bd.fixedRotation := true;
  bd.position.&Set(-10.0, y);
  body1 := m_world.CreateBody(@bd);
  body1.CreateFixture(shape, 5.0);

  bd.position.&Set(10.0, y);
  body2 := m_world.CreateBody(@bd);
  body2.CreateFixture(shape, 5.0);

  pulleyDef := b2PulleyJointDef.Create;
  anchor1 := b2Vec2.Create(-10.0, y + b);
  anchor2 := b2Vec2.Create(10.0, y + b);
  groundAnchor1 := b2Vec2.Create(-10.0, y + b + L);
  groundAnchor2 := b2Vec2.Create(10.0, y + b + L);
  pulleyDef.Initialize(body1, body2, groundAnchor1, groundAnchor2, anchor1, anchor2, 1.5);

  m_joint1 := m_world.CreateJoint(@pulleyDef);

end;

class function TPulleys.CreateTest: TTest;
begin
  Result := TPulleys.Create;
end;

procedure TPulleys.Step(settings: PSettings);
var
  ratio, L: Float32;
begin
  inherited Step(settings);

  ratio := m_joint1.GetRatio();
  L := m_joint1.GetCurrentLengthA + ratio * m_joint1.GetCurrentLengthB;
  g_debugDraw.DrawString(5, m_textLine, 'L1 + %4.2f * L2 = %4.2f', [ratio, L]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Pulleys', @TPulleys.CreateTest));
end.

