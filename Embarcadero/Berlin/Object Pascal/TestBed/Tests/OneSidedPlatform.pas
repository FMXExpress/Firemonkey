//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit OneSidedPlatform;

interface

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, Test;

type
  TOneSidedPlatform = class(TTest)
  protected
  type
    TState = (e_unknown, e_above, e_below);
  const
    b2_linearSlop = 0.005;
  protected
    m_radius, m_top, m_bottom: float32;
    m_state: TState;
    m_platform: pb2Fixture;
    m_character: pb2Fixture;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold); override; cdecl;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  DebugDraw;

{ TOneSidedPlatform }

constructor TOneSidedPlatform.Create;
var
  ground, body: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  bd: b2BodyDef;
  fd: b2FixtureDef;
  polyShape: b2PolygonShapeWrapper;
  circleShape: b2CircleShapeWrapper;
begin
  inherited Create;

  // Ground

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;
  edgeShape.&Set(b2Vec2.Create(-20.0, 0.0), b2Vec2.Create(20.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);

  edgeShape.Destroy;

  // Platform
  bd := b2BodyDef.Create;
  bd.position.&Set(0.0, 10.0);
  body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(3.0, 0.5);
  m_platform := body.CreateFixture(polyShape, 0.0);

  m_bottom := 10.0 - 0.5;
  m_top := 10.0 + 0.5;

  polyShape.Destroy;

  // Actor
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 12.0);
  body := m_world.CreateBody(@bd);

  m_radius := 0.5;
  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := m_radius;
  m_character := body.CreateFixture(circleShape, 20.0);
  circleShape.Destroy;

  body.SetLinearVelocity(b2Vec2.Create(0.0, -50.0));
  m_state := e_unknown;
end;

class function TOneSidedPlatform.CreateTest: TTest;
begin
  Result := TOneSidedPlatform.Create;
end;

{$DEFINE OPTION_1}
procedure TOneSidedPlatform.PreSolve(contact: b2ContactHandle; oldManifold: Pb2Manifold);
var
  fixtureA, fixtureB: pb2Fixture;
  LContact: b2ContactWrapper;
  body: b2BodyWrapper;
{$IFDEF OPTION_1}
  position: b2Vec2;
{$ELSE}
  v: b2Vec2;
{$ENDIF OPTION_1}
begin
  inherited PreSolve(contact, oldManifold);

  LContact := contact;

  fixtureA := LContact.GetFixtureA;
  fixtureB := LContact.GetFixtureB;

  if (fixtureA <> m_platform) and (fixtureA <> m_character) then
    Exit;

  if (fixtureB <> m_platform) and (fixtureB <> m_character) then
    Exit;

  body := m_character.GetBody;
{$IFDEF OPTION_1}
  position := body.GetPosition^;

  if (position.y < m_top + m_radius - 3.0 * b2_linearSlop) then
    LContact.SetEnabled(False);
{$ELSE}
  v := body.GetLinearVelocity^;
  if (v.y > 0.0) then
    LContact.SetEnabled(False);

{$ENDIF OPTION_1}

end;

procedure TOneSidedPlatform.Step(settings: PSettings);
var
  v: b2Vec2;
  body: b2BodyWrapper;
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Press: (c) create a shape, (d) destroy a shape.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  body := m_character.GetBody;
  v := body.GetLinearVelocity^;
  g_debugDraw.DrawString(5, m_textLine, 'Character Linear Velocity: %f?', [v.y]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

end;

initialization
  RegisterTest(TestEntry.Create('OneSidedPlatform', @TOneSidedPlatform.CreateTest));
end.

