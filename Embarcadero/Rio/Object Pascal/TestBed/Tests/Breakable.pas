//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Breakable;

interface

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, Test;

type
  TBreakable = class(TTest)
  protected
    m_body1: b2BodyWrapper;
    m_velocity: b2Vec2;
    m_angularVelocity: float32;
    m_shape1: b2PolygonShapeWrapper;
    m_shape2: b2PolygonShapeWrapper;
    m_piece1: Pb2Fixture;
    m_piece2: Pb2Fixture;

    m_broke: Boolean;
    m_break: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    class function CreateTest: TTest; static;

    procedure Break;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
    procedure PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse); override; cdecl;
end;

implementation

uses
  System.Math,
  DebugDraw;


procedure TBreakable.Break;
var
  body1: b2BodyWrapper;
  body2: b2BodyWrapper;
  center: b2Vec2;
  center1: b2Vec2;
  center2: b2Vec2;
  velocity1: b2Vec2;
  velocity2: b2Vec2;
  bd: b2BodyDef;
begin
  // Create two bodies from one.
  body1 := m_piece1.GetBody;
  center := body1.GetWorldCenter^;

  body1.DestroyFixture(m_piece2);
  m_piece2 := nil;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position := body1.GetPosition^;
  bd.angle := body1.GetAngle;

  body2 := m_world.CreateBody(@bd);
  m_piece2 := body2.CreateFixture(m_shape2, 1.0);

  // Compute consistent velocities for new bodies based on
  // cached velocity.
  center1 := body1.GetWorldCenter^;
  center2 := body2.GetWorldCenter^;

  velocity1 := m_velocity + b2Cross(m_angularVelocity, center1 - center);
  velocity2 := m_velocity + b2Cross(m_angularVelocity, center2 - center);

  body1.SetAngularVelocity(m_angularVelocity);
  body1.SetLinearVelocity(velocity1);

  body2.SetAngularVelocity(m_angularVelocity);
  body2.SetLinearVelocity(velocity2);
end;

constructor TBreakable.Create;
var
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  shape: b2EdgeShapeWrapper;
begin
  inherited;

  // Ground body
  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  shape := b2EdgeShapeWrapper.Create;
  shape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(shape, 0.0);
  shape.Destroy;

  // Breakable dynamic body
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 40.0);
  bd.angle := 0.25 * pi;
  m_body1 := m_world.CreateBody(@bd);

  m_shape1 := b2PolygonShapeWrapper.Create;
  m_shape1.SetAsBox(0.5, 0.5, b2Vec2.Create(-0.5, 0.0), 0.0);
  m_piece1 := m_body1.CreateFixture(m_shape1, 1.0);

  m_shape2 := b2PolygonShapeWrapper.Create;
  m_shape2.SetAsBox(0.5, 0.5, b2Vec2.Create(0.5, 0.0), 0.0);
  m_piece2 := m_body1.CreateFixture(m_shape2, 1.0);

  m_break := false;
  m_broke := false;
end;

class function TBreakable.CreateTest: TTest;
begin
  Result := TBreakable.Create;
end;

destructor TBreakable.Destroy;
begin
  m_shape1.Destroy;
  m_shape2.Destroy;

  inherited;
end;

procedure TBreakable.Keyboard(key: Integer);
begin
  inherited;

end;

procedure TBreakable.PostSolve(contact: b2ContactHandle; impulse: Pb2ContactImpulse);
var
  maxImpulse: float32;
  count: Integer;
  LContact: b2ContactWrapper;
  I: Integer;
begin
  // The body already broke.
	if m_broke then
    Exit;

  // Should the body break?
  LContact := contact;
  count := LContact.GetManifold.pointCount;

  maxImpulse := 0.0;
  for I := 0 to count -1 do
  begin
    maxImpulse := Max(maxImpulse, impulse.normalImpulses[I]);
  end;

  if maxImpulse > 40.0 then
  begin
    // Flag the body for breaking.
    m_break := true;
  end;
end;

procedure TBreakable.Step(settings: PSettings);
begin
	if (m_break) then
  begin
    Break;
    m_broke := true;
    m_break := false;
  end;

  // Cache velocities to improve movement on breakage.
  if (m_broke = false) then
  begin
    m_velocity := m_body1.GetLinearVelocity^;
    m_angularVelocity := m_body1.GetAngularVelocity;
  end;

  inherited Step(settings);
end;

initialization
  RegisterTest(TestEntry.Create('Breakable', @TBreakable.CreateTest));

end.

