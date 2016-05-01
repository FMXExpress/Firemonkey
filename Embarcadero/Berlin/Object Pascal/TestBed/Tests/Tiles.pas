//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Tiles;

interface

uses
  Test;

type

  /// This stress tests the dynamic tree broad-phase. This also shows that tile
  /// based collision is _not_ smooth due to Box2D not knowing about adjacency.
  TTiles = class(TTest)
  const
    e_count = 20;
  private
  	m_fixtureCount: Integer;
	  m_createTime: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;
    procedure Step(settings: PSettings); override;
  end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TTiles }

constructor TTiles.Create;
const
  N = 200;
  M = 10;
var
  I, J: Integer;
  a: Float32;
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  position: b2Vec2;
  shape: b2PolygonShapeWrapper;
  x, y, deltaX, deltaY: b2Vec2;
  timer: b2Timer;
begin
  inherited;

	m_fixtureCount := 0;
  timer := b2Timer.Create;

	a := 0.5;
  bd := b2BodyDef.Create;
	bd.position.y := -a;
	ground := m_world.CreateBody(@bd);

{$IF TRUE}
  position := b2Vec2.Create;
  position.y := 0.0;
  for I := 0 to M - 1 do
  begin
    position.x := -N * a;
    for J := 0 to N - 1 do
    begin
      shape := b2PolygonShapeWrapper.Create;
      shape.SetAsBox(a, a, position, 0.0);
      ground.CreateFixture(&shape, 0.0);
      Inc(m_fixtureCount);
      position.x := position.x  + 2.0 * a;
      shape.Destroy;
    end;
    position.y := position.y - 2.0 * a;
  end;
{$ELSE}
  position := b2Vec2.Create;
  position.x := -N * a;
  for I := 0 to N - 1 do
  begin
    position.y := 0.0;
    for J := 0 to M - 1 do
    begin
      shape := b2PolygonShapeWrapper.Create;
      shape.SetAsBox(a, a, position, 0.0);
      ground.CreateFixture(&shape, 0.0);
      position.y := position.y  - 2.0 * a;
      shape.Destroy;
    end;
    position.x := position.x + 2.0 * a;
  end;
{$ENDIF}

  a := 0.5;
  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(a, a);

  x := b2Vec2.Create(-7.0, 0.75);
  y := b2Vec2.Create;
  deltaX := b2Vec2.Create(0.5625, 1.25);
  deltaY := b2Vec2.Create(1.125, 0.0);

  for I := 0 to e_count - 1 do
  begin
    y := x;

    for j := i to e_count -1 do
    begin
      bd := b2BodyDef.Create;
      bd.&type := b2_dynamicBody;
      bd.position := y;

      //if (i = 0) and (j = 0) then
      //	bd.allowSleep := false
      //else
      //	bd.allowSleep := true;

      body := m_world.CreateBody(@bd);
      body.CreateFixture(shape, 5.0);
      Inc(m_fixtureCount);
      y  := y + deltaY;
    end;

    x := x + deltaX;
  end;
  shape.Destroy;

  m_createTime := timer.GetMilliseconds;
end;

class function TTiles.CreateTest: TTest;
begin
  Result := TTiles.Create;
end;

procedure TTiles.Step(settings: PSettings);
var
  cm: b2ContactManagerWrapper;
  LBroadPhase: b2BroadPhaseWrapper;
  minimumNodeCount: int32;
  height: int32;
  leafCount: int32;
  minimumHeight: float32;
begin

  cm := m_world.GetContactManager;
  LBroadPhase := cm.m_broadPhase;
  height := LBroadPhase.GetTreeHeight;
  leafCount := LBroadPhase.GetProxyCount;
  minimumNodeCount := 2 * leafCount - 1;
  minimumHeight := Round(Log10(minimumNodeCount) / Log10(2.0));
  g_debugDraw.DrawString(5, m_textLine, 'dynamic tree height = %d, min = %d', [height, Trunc(minimumHeight)]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  inherited Step(settings);

  g_debugDraw.DrawString(5, m_textLine, 'create time = %6.2f ms, fixture count = %d', [m_createTime, m_fixtureCount]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Tiles', @TTiles.CreateTest));

end.
