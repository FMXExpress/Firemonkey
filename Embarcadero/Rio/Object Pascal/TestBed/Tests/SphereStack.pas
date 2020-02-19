//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit SphereStack;

interface

uses
  Box2D.Dynamics, Test;

type
  TSphereStack = class(TTest)
  protected
  const
		e_count = 10;
  protected
	  m_bodies: array [0..e_count-1] of b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TSphereStack }

constructor TSphereStack.Create;
var
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  circleShape: b2CircleShapeWrapper;
  I: Integer;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);
  edgeShape.Destroy;

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 1.0;

  for I := 0 to e_count - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(0.0, 4.0 + 3.0 * I);

    m_bodies[I] := m_world.CreateBody(@bd);

    m_bodies[I].CreateFixture(circleShape, 1.0);

    m_bodies[I].SetLinearVelocity(b2Vec2.Create(0.0, -50.0));
  end;
  circleShape.Destroy;

end;

class function TSphereStack.CreateTest: TTest;
begin
  Result := TSphereStack.Create;
end;

procedure TSphereStack.Step(settings: PSettings);
begin
  inherited Step(settings);

  //for (int32 i = 0; i < e_count; ++i)
  //{
  //	printf("%g ", m_bodies[i]->GetWorldCenter().y);
  //}

  //for (int32 i = 0; i < e_count; ++i)
  //{
  //	printf("%g ", m_bodies[i]->GetLinearVelocity().y);
  //}

  //printf("\n");
end;

initialization
  RegisterTest(TestEntry.Create('SphereStack', @TSphereStack.CreateTest));
end.
