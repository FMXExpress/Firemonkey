//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Mobile;

interface

uses
  Box2D.Common, Box2D.Dynamics, Test;

type
  TMobile = class(TTest)
  protected
  const
		e_depth = 4;

  protected

  	function AddNode(const parent: b2BodyWrapper; const localAnchor: b2Vec2; depth: Integer;
      offset, a: float32): b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

end;

implementation

uses
  System.Math,
  Box2D.Collision, DebugDraw;


{ TMobile }

constructor TMobile.Create;
var
  ground, root: b2BodyWrapper;
  bodyDef: b2BodyDef;
  a: Float32;
  h: b2Vec2;
  jointDef: b2RevoluteJointDef;

begin
  inherited Create;

  // Create ground body.
  bodyDef := b2BodyDef.Create;
  bodyDef.position.&Set(0.0, 20.0);
  ground := m_world.CreateBody(@bodyDef);

  a := 0.5;
  h := b2Vec2.Create(0.0, a);

  root := AddNode(ground, b2Vec2.Create(0.0, 0.0), 0, 3.0, a);

  jointDef := b2RevoluteJointDef.Create;
  jointDef.bodyA := ground;
  jointDef.bodyB := root;
  jointDef.localAnchorA.SetZero;
  jointDef.localAnchorB := h;
  m_world.CreateJoint(@jointDef);
end;

function TMobile.AddNode(const parent: b2BodyWrapper; const localAnchor: b2Vec2; depth: Integer;
  offset, a: float32): b2BodyWrapper;
var
  h, p, a1, a2: b2Vec2;
  bodyDef: b2BodyDef;
  density: Float32;
  body1, body2: b2BodyWrapper;
  shape: b2PolygonShapeWrapper;
  jointDef: b2RevoluteJointDef;

begin
  density := 20.0;
  h := b2Vec2.Create(0.0, a);

  p := parent.GetPosition^ + localAnchor - h;

  bodyDef := b2BodyDef.Create;
  bodyDef.&type := b2_dynamicBody;
  bodyDef.position := p;
  Result := m_world.CreateBody(@bodyDef);

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(0.25 * a, a);
  Result.CreateFixture(shape, density);
  shape.Destroy;

  if depth < e_depth then
  begin
    a1 := b2Vec2.Create(offset, -a);
    a2 := b2Vec2.Create(-offset, -a);
    body1 := AddNode(Result, a1, depth + 1, 0.5 * offset, a);
    body2 := AddNode(Result, a2, depth + 1, 0.5 * offset, a);

    jointDef := b2RevoluteJointDef.Create;
    jointDef.bodyA := Result;
    jointDef.localAnchorB := h;

    jointDef.localAnchorA := a1;
    jointDef.bodyB := body1;
    m_world.CreateJoint(@jointDef);

    jointDef.localAnchorA := a2;
    jointDef.bodyB := body2;
    m_world.CreateJoint(@jointDef);
  end;
end;


class function TMobile.CreateTest: TTest;
begin
  Result := TMobile.Create;
end;

initialization
  RegisterTest(TestEntry.Create('Mobile', @TMobile.CreateTest));
end.

