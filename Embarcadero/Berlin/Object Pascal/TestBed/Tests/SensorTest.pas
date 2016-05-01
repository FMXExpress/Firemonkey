//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit SensorTest;

interface

uses
  Box2D.Dynamics, Test;

type
  TSensorTest = class(TTest)
  protected
  const
		e_count = 7;
    FLT_EPSILON = 1.19209290E-07;
  protected
    m_sensor: pb2Fixture;
    m_bodies: array [0..e_count-1] of b2BodyWrapper;
    m_touching: array [0..e_count-1] of Boolean;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure BeginContact(contact: b2ContactHandle); override; cdecl;
    procedure EndContact(contact: b2ContactHandle); override; cdecl;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TSensorTest }


{$DEFINE OPTION_1}
constructor TSensorTest.Create;
var
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  fd: b2FixtureDef;
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

{$IFNDEF OPTION_1}
  fd := b2FixtureDef.Create;
  fd.SetAsBox(10.0, 2.0, b2Vec2.Create(0.0, 20.0), 0.0);
  fd.isSensor := True;
  m_sensor := ground.CreateFixture(@fd);
{$ELSE}
  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 5.0;
  circleShape.m_p := b2vec2.Create(0.0, 10.0);

  fd := b2FixtureDef.Create;
  fd.&shape := circleShape;
  fd.isSensor := true;
  m_sensor := ground.CreateFixture(@fd);
  circleShape.Destroy;
{$ENDIF}

  circleShape := b2CircleShapeWrapper.Create;
  circleShape.m_radius := 1.0;

  for I := 0 to e_count - 1 do
  begin
    bd := b2BodyDef.Create;
    bd.&type := b2_dynamicBody;
    bd.position.&Set(-10.0 + 3.0 * I, 20.0);
    bd.userData := @m_touching[I];

    m_touching[I] := False;
    m_bodies[i] := m_world.CreateBody(@bd);

    m_bodies[i].CreateFixture(circleShape, 1.0);
  end;
  circleShape.Destroy;
end;

class function TSensorTest.CreateTest: TTest;
begin
  Result := TSensorTest.Create;
end;

procedure TSensorTest.BeginContact(contact: b2ContactHandle);
var
  LContact: b2ContactWrapper;
  LBody: b2BodyWrapper;
  fixtureA, fixtureB: Pb2Fixture;
  userData: Pointer;
begin
  LContact := contact;
  fixtureA := LContact.GetFixtureA;
  fixtureB := LContact.GetFixtureB;

  if (fixtureA = m_sensor) then
  begin
    LBody := fixtureB.GetBody;
    userData := LBody.GetUserData;
    if (userData <> nil) then
      PBoolean(userData)^ := True;
  end;

  if (fixtureB = m_sensor) then
  begin
    LBody := fixtureB.GetBody;
    userData := LBody.GetUserData;
    if (userData <> nil) then
      PBoolean(userData)^ := True;
  end;
end;

procedure TSensorTest.EndContact(contact: b2ContactHandle);
var
  LContact: b2ContactWrapper;
  LBody: b2BodyWrapper;
  fixtureA, fixtureB: Pb2Fixture;
  userData: Pointer;
begin
  LContact := contact;
  fixtureA := LContact.GetFixtureA;
  fixtureB := LContact.GetFixtureB;

  if (fixtureA = m_sensor) then
  begin
    LBody := fixtureB.GetBody;
    userData := LBody.GetUserData;
    if (userData <> nil) then
      PBoolean(userData)^ := False;
  end;

  if (fixtureB = m_sensor) then
  begin
    LBody := fixtureB.GetBody;
    userData := LBody.GetUserData;
    if (userData <> nil) then
      PBoolean(userData)^ := False;
  end;
end;

procedure TSensorTest.Step(settings: PSettings);
var
  I: Integer;
  ground, body: b2BodyWrapper;
  circle: b2CircleShapeWrapper;
  center, position, d, F: b2Vec2;
begin
  inherited Step(settings);

  // Traverse the contact results. Apply a force on shapes
  // that overlap the sensor.
  for I := 0 to e_count - 1 do
  begin
    if (m_touching[i] = True) then
    begin
      body := m_bodies[i];
      ground := m_sensor.GetBody;

      circle := m_sensor.GetShape;
      center := ground.GetWorldPoint(circle.m_p);

      position := body.GetPosition^;

      d := center - position;
      if (d.LengthSquared() < FLT_EPSILON * FLT_EPSILON) then
      begin
        Continue;
      end;

      d.Normalize();
      F := 100.0 * d;
      body.ApplyForce(F, position, false);
    end;
  end;
end;

initialization
  RegisterTest(TestEntry.Create('SensorTest', @TSensorTest.CreateTest));
end.
