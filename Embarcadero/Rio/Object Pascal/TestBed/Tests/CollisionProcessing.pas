//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit CollisionProcessing;

interface

uses
  Test;

type
  TCollisionProcessing = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math, System.Generics.Collections,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TCollisionProcessing.Create;
var
  edgeShape: b2EdgeShapeWrapper;
  sd: b2FixtureDef;
  bd: b2BodyDef;
  xLo, xHi: Float32;
  yLo, yHi: Float32;
  vertices: array [0..2] of b2Vec2;
  polyShape: b2PolygonShapeWrapper;
  triangleShapeDef: b2FixtureDef;
  triangleBodyDef: b2BodyDef;
  ground, body1, body2, body3, body4, body5, body6: b2BodyWrapper;
  boxShapeDef: b2FixtureDef;
  boxBodyDef: b2BodyDef;
  circle: b2CircleShapeWrapper;
  circleShapeDef: b2FixtureDef;
  circleBodyDef: b2BodyDef;

begin
  inherited Create;

  // Ground body
  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-50.0, 0.0), b2Vec2.Create(50.0, 0.0));

  sd := b2FixtureDef.Create;
  sd.shape := edgeShape;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);
  ground.CreateFixture(@sd);

  xLo := -5.0;
  xHi := 5.0;
  yLo := 2.0;
  yHi := 35.0;

  // Small triangle
  vertices[0].&Set(-1.0, 0.0);
  vertices[1].&Set(1.0, 0.0);
  vertices[2].&Set(0.0, 2.0);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.&Set(@vertices[0], 3);

  triangleShapeDef := b2FixtureDef.Create;
  triangleShapeDef.shape := polyShape;
  triangleShapeDef.density := 1.0;

  triangleBodyDef := b2BodyDef.Create;
  triangleBodyDef.&type := b2_dynamicBody;
  triangleBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body1 := m_world.CreateBody(@triangleBodyDef);
  body1.CreateFixture(@triangleShapeDef);

  // Large triangle (recycle definitions)
  vertices[0] := vertices[0] * 2.0;
  vertices[1] := vertices[1] * 2.0;
  vertices[2] := vertices[2] * 2.0;
  polyShape.&Set(@vertices[0], 3);

  triangleBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body2 := m_world.CreateBody(@triangleBodyDef);
  body2.CreateFixture(@triangleShapeDef);

  // Small box
  polyShape.SetAsBox(1.0, 0.5);

  boxShapeDef := b2FixtureDef.Create;
  boxShapeDef.shape := polyShape;
  boxShapeDef.density := 1.0;

  boxBodyDef := b2BodyDef.Create;
  boxBodyDef.&type := b2_dynamicBody;
  boxBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body3 := m_world.CreateBody(@boxBodyDef);
  body3.CreateFixture(@boxShapeDef);

  // Large box (recycle definitions)
  polyShape.SetAsBox(2.0, 1.0);
  boxBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body4 := m_world.CreateBody(@boxBodyDef);
  body4.CreateFixture(@boxShapeDef);

  // Small circle
  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 1.0;

  circleShapeDef := b2FixtureDef.Create;
  circleShapeDef.shape := circle;
  circleShapeDef.density := 1.0;

  circleBodyDef := b2BodyDef.Create;
  circleBodyDef.&type := b2_dynamicBody;
  circleBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body5 := m_world.CreateBody(@circleBodyDef);
  body5.CreateFixture(@circleShapeDef);

  // Large circle
  circle.m_radius := circle.m_radius * 2.0;
  circleBodyDef.position.&Set(RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

  body6 := m_world.CreateBody(@circleBodyDef);
  body6.CreateFixture(@circleShapeDef);
end;

class function TCollisionProcessing.CreateTest: TTest;
begin
  Result := TCollisionProcessing.Create;
end;

procedure TCollisionProcessing.Step(settings: PSettings);
const
  k_maxNuke = 8;
var
  nuke: array [0..k_maxNuke-1] of THandle;
  nukeCount: Integer;
  I: Integer;
  h: THandle;
  b, body1, body2: b2BodyWrapper;
  mass1, mass2: float32;
begin
  inherited Step(settings);

  // We are going to destroy some bodies according to contact
  // points. We must buffer the bodies that should be destroyed
  // because they may belong to multiple contact points.
  nukeCount := 0;

  FillChar(Nuke, Sizeof(THandle)*Length(Nuke), 0);

  // Traverse the contact results. Destroy bodies that
  // are touching heavier bodies.
  for I := 0 to m_pointCount - 1 do
  begin
    body1 := m_points[I].fixtureA.GetBody;
    body2 := m_points[I].fixtureB.GetBody;
    mass1 := body1.GetMass;
    mass2 := body2.GetMass;

    if (mass1 > 0.0) and (mass2 > 0.0) then
    begin
      if (mass2 > mass1) then
        nuke[nukeCount] := body1.FHandle
      else
        nuke[nukeCount] := body2.FHandle;

      Inc(nukeCount);

      if (nukeCount = k_maxNuke) then
        Break;
    end;
  end;

  // Sort the nuke array to group duplicates.
  //std::sort(nuke, nuke + nukeCount);
  TArray.Sort<THandle>(nuke);

  // Destroy the bodies, skipping duplicates.
  I := 0;
  while (I < k_maxNuke) do
  begin
    h := nuke[I];
    Inc(I);
    while ((I < k_maxNuke) and (nuke[I] = h)) do
      Inc(I);

    if (h <> 0) and (h <> m_bomb.FHandle) then
      m_world.DestroyBody(h);
  end;
end;

initialization
  RegisterTest(TestEntry.Create('CollisionProcessing', @TCollisionProcessing.CreateTest));
end.

