//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit HeavyOnLightTwo;

interface

uses
  Box2D.Dynamics, Test;

type
  THeavyOnLightTwo = class(TTest)
  protected
    m_heavy: b2BodyWrapper;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;

    procedure ToggleHeavy;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;


constructor THeavyOnLightTwo.Create;
var
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  circle: b2CircleShapeWrapper;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edge, 0.0);


  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 2.5);
  body := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.m_radius := 0.5;
  body.CreateFixture(circle, 10.0);

  bd.position.&Set(0.0, 3.5);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(circle, 10.0);

  //m_heavy := NULL;
end;

class function THeavyOnLightTwo.CreateTest: TTest;
begin
  Result := THeavyOnLightTwo.Create;
end;

procedure THeavyOnLightTwo.Keyboard(key: Integer);
begin
  case key of
    Ord('H'), Ord('h'):
      ToggleHeavy;
  end;
end;

procedure THeavyOnLightTwo.ToggleHeavy;
var
  bd: b2BodyDef;
  shape: b2CircleShapeWrapper;
begin
  if (m_heavy.FHandle <> 0) then
  begin
    m_world.DestroyBody(m_heavy);
    m_heavy.FHandle := 0;
  end
  else
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(0.0, 9.0);
    m_heavy := m_world.CreateBody(@bd);

    shape := b2CircleShapeWrapper.Create;
    shape.m_radius := 5.0;
    m_heavy.CreateFixture(shape, 10.0);
    shape.Destroy;
  end;
end;

procedure THeavyOnLightTwo.Step(settings: PSettings);
begin
  g_debugDraw.DrawString(5, m_textLine, 'Press "h" to toggle Heavy circle');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  inherited Step(settings);
end;

initialization
  RegisterTest(TestEntry.Create('HeavyOnLightTwo', @THeavyOnLightTwo.CreateTest));
end.

