//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Pyramid;

interface

uses
  Test;

type
  TPyramid = class(TTest)
  protected
  const
    e_count = 20;

  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TPyramid }

constructor TPyramid.Create;
var
  I, J: Integer;
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;
  a: Float32;
  x, y, deltaX, deltaY: b2Vec2;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  //
  a := 0.5;
  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(a, a);

  x := b2Vec2.Create(-7.0, 0.75);
  deltaX := b2Vec2.Create(0.5625, 1.25);
  deltaY := b2Vec2.Create(1.125, 0.0);

  for I := 0 to e_count - 1 do
  begin
    y := x;
    for J := I to e_count - 1 do
    begin
      bd := b2BodyDef.Create;
      bd.&type := b2_dynamicBody;
      bd.position := y;
      body := m_world.CreateBody(@bd);
      body.CreateFixture(polyShape, 5.0);
      y := y + deltaY;
    end;
    x := x + deltaX;
  end;
  polyShape.Destroy;

end;

class function TPyramid.CreateTest: TTest;
begin
  Result := TPyramid.Create;
end;

procedure TPyramid.Step(settings: PSettings);
begin
  inherited Step(settings);
  //b2DynamicTree* tree = &m_world->m_contactManager.m_broadPhase.m_tree;

  //if (m_stepCount == 400)
  //{
  //	tree->RebuildBottomUp();
  //}

end;

initialization
  RegisterTest(TestEntry.Create('Pyramid', @TPyramid.CreateTest));
end.

