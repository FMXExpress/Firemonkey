//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit CollisionFiltering;

interface

uses
  Test;

type
  TCollisionFiltering = class(TTest)
  protected
    const
      k_smallGroup = 1;
      k_largeGroup = -1;
      k_defaultCategory = $0001;
      k_triangleCategory = $0002;
      k_boxCategory = $0004;
      k_circleCategory = $0008;
      k_triangleMask = $FFFF;
      k_boxMask = $FFFF xor k_triangleCategory;
      k_circleMask = $FFFF;

  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TCollisionFiltering.Create;
var
  edgeShape: b2EdgeShapeWrapper;
  ground, body, body1, body2, body3, body4, body5, body6: b2BodyWrapper;
  bd: b2BodyDef;
  sd: b2FixtureDef;
  vertices: array [0..2] of b2Vec2;
  polyShape: b2PolygonShapeWrapper;
  triangleShapeDef: b2FixtureDef;
  triangleBodyDef: b2BodyDef;
  jd: b2PrismaticJointDef;
  boxShapeDef: b2FixtureDef;
  boxBodyDef : b2BodyDef;
  circle: b2CircleShapeWrapper;
  circleShapeDef: b2FixtureDef;
  circleBodyDef: b2BodyDef;

begin
  inherited Create;

  // Ground body
  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));

  sd := b2FixtureDef.Create;
  sd.shape := edgeShape;
  sd.friction := 0.3;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(@sd);
  edgeShape.Destroy;

  // Small triangle
  vertices[0].&Set(-1.0, 0.0);
  vertices[1].&Set(1.0, 0.0);
  vertices[2].&Set(0.0, 2.0);
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.&Set(@vertices[0], 3);

  triangleShapeDef := b2FixtureDef.Create;
  triangleShapeDef.shape := polyShape;
  triangleShapeDef.density := 1.0;

  triangleShapeDef.filter.groupIndex := k_smallGroup;
  triangleShapeDef.filter.categoryBits := k_triangleCategory;
  triangleShapeDef.filter.maskBits := k_triangleMask;

  triangleBodyDef := b2BodyDef.Create;
  triangleBodyDef.&type := b2_dynamicBody;
  triangleBodyDef.position.&Set(-5.0, 2.0);

  body1 := m_world.CreateBody(@triangleBodyDef);
  body1.CreateFixture(@triangleShapeDef);

  // Large triangle (recycle definitions)
  vertices[0] := vertices[0] * 2.0;
  vertices[1] := vertices[1] * 2.0;
  vertices[2] := vertices[2] * 2.0;
  polyShape.&Set(@vertices[0], 3);
  triangleShapeDef.filter.groupIndex := k_largeGroup;
  triangleBodyDef.position.&Set(-5.0, 6.0);
  triangleBodyDef.fixedRotation := true; // look at me!

  body2 := m_world.CreateBody(@triangleBodyDef);
  body2.CreateFixture(@triangleShapeDef);

  polyShape.Destroy;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(-5.0, 10.0);
  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(0.5, 1.0);
  body.CreateFixture(polyShape, 1.0);

  jd := b2PrismaticJointDef.Create;
  jd.bodyA := body2;
  jd.bodyB := body;
  jd.enableLimit := true;
  jd.localAnchorA.&Set(0.0, 4.0);
  jd.localAnchorB.SetZero();
  jd.localAxisA.&Set(0.0, 1.0);
  jd.lowerTranslation := -1.0;
  jd.upperTranslation := 1.0;

  m_world.CreateJoint(@jd);

  // Small box
  polyShape.SetAsBox(1.0, 0.5);
  boxShapeDef := b2FixtureDef.Create;
  boxShapeDef.shape := polyShape;
  boxShapeDef.density := 1.0;
  boxShapeDef.restitution := 0.1;

  boxShapeDef.filter.groupIndex := k_smallGroup;
  boxShapeDef.filter.categoryBits := k_boxCategory;
  boxShapeDef.filter.maskBits := k_boxMask;

  boxBodyDef := b2BodyDef.Create;
  boxBodyDef.&type := b2_dynamicBody;
  boxBodyDef.position.&Set(0.0, 2.0);

  body3 := m_world.CreateBody(@boxBodyDef);
  body3.CreateFixture(@boxShapeDef);

  // Large box (recycle definitions)
  polyShape.SetAsBox(2.0, 1.0);
  boxShapeDef.filter.groupIndex := k_largeGroup;
  boxBodyDef.position.&Set(0.0, 6.0);

  body4 := m_world.CreateBody(@boxBodyDef);
  body4.CreateFixture(@boxShapeDef);

  polyShape.Destroy;

  // Small circle
  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 1.0;

  circleShapeDef := b2FixtureDef.Create;
  circleShapeDef.shape := circle;
  circleShapeDef.density := 1.0;

  circleShapeDef.filter.groupIndex := k_smallGroup;
  circleShapeDef.filter.categoryBits := k_circleCategory;
  circleShapeDef.filter.maskBits := k_circleMask;

  circleBodyDef := b2BodyDef.Create;
  circleBodyDef.&type := b2_dynamicBody;
  circleBodyDef.position.&Set(5.0, 2.0);

  body5 := m_world.CreateBody(@circleBodyDef);
  body5.CreateFixture(@circleShapeDef);

  // Large circle
  circle.m_radius := circle.m_radius * 2.0;
  circleShapeDef.filter.groupIndex := k_largeGroup;
  circleBodyDef.position.&Set(5.0, 6.0);

  body6 := m_world.CreateBody(@circleBodyDef);
  body6.CreateFixture(@circleShapeDef);
  circle.Destroy;
end;

class function TCollisionFiltering.CreateTest: TTest;
begin
  Result := TCollisionFiltering.Create;
end;

initialization
  RegisterTest(TestEntry.Create('CollisionFiltering', @TCollisionFiltering.CreateTest));
end.

