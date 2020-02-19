//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Confined;

interface

uses
  Test;

type
  TConfined = class(TTest)
  protected
  const
		e_columnCount = 0;
		e_rowCount = 0;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure CreateCircle;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TConfined.Create;
var
  bd: b2BodyDef;
  ground: b2BodyWrapper;
  edgeShape: b2EdgeShapeWrapper;
  circle: b2CircleShapeWrapper;
  fd: b2FixtureDef;
  I: Integer;
  radius: Float32;
  J: Integer;
  body: b2BodyWrapper;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeShape := b2EdgeShapeWrapper.Create;

  // Floor
  edgeShape.&Set(b2Vec2.Create(-10.0, 0.0), b2Vec2.Create(10.0, 0.0));
  ground.CreateFixture(edgeShape, 0.0);

  // Left wall
  edgeShape.&Set(b2Vec2.Create(-10.0, 0.0), b2Vec2.Create(-10.0, 20.0));
  ground.CreateFixture(edgeShape, 0.0);

  // Right wall
  edgeShape.&Set(b2Vec2.Create(10.0, 0.0), b2Vec2.Create(10.0, 20.0));
  ground.CreateFixture(edgeShape, 0.0);

  // Roof
  edgeShape.&Set(b2Vec2.Create(-10.0, 20.0), b2Vec2.Create(10.0, 20.0));
  ground.CreateFixture(edgeShape, 0.0);


  radius := 0.5;
  circle := b2CircleShapeWrapper.Create;
  circle.m_p := b2Vec2.Create(0.0, 0.0);
  circle.m_radius := radius;

  fd := b2FixtureDef.Create;
  fd.shape := circle;
  fd.density := 1.0;
  fd.friction := 0.1;

  for J := 0 to e_columnCount - 1 do
  begin
    for I := 0 to e_rowCount - 1 do
    begin
      bd := b2BodyDef.Create;
      bd.&type := b2_dynamicBody;
      bd.position.&Set(-10.0 + (2.1 * J + 1.0 + 0.01 * I) * radius, (2.0 * I + 1.0) * radius);
      body := m_world.CreateBody(@bd);
      body.CreateFixture(@fd);
    end;
  end;

  m_world.SetGravity(b2Vec2.Create(0.0, 0.0));

end;

procedure TConfined.CreateCircle;
var
  radius: Float32;
  shape: b2CircleShapeWrapper;
  fd: b2FixtureDef;
  bd: b2BodyDef;
  p: b2Vec2;
  body: b2BodyWrapper;

begin
  radius := 2.0;
  shape := b2CircleShapeWrapper.Create;
  shape.m_p := b2vec2.Create(0.0, 0.0);
  shape.m_radius := radius;

  fd := b2FixtureDef.Create;
  fd.shape := shape;
  fd.density := 1.0;
  fd.friction := 0.0;

  p := b2Vec2.Create(RandomFloat(), 3.0 + RandomFloat());
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position := p;
  //bd.allowSleep := false;
  body := m_world.CreateBody(@bd);

  body.CreateFixture(@fd);
end;

class function TConfined.CreateTest: TTest;
begin
  Result := TConfined.Create;
end;

procedure TConfined.Keyboard(key: Integer);
begin
  case key of
    Ord('C'), Ord('c'):
      CreateCircle();
  end;
end;

procedure TConfined.Step(settings: PSettings);
var
  sleeping: Boolean;
  b: b2BodyWrapper;
  p: pb2Vec2;
begin
  sleeping := true;

  b := m_world.GetBodyList;

  while (b.FHandle <> 0) do
  begin
    if (b.GetType = b2_dynamicBody) then
    begin
      if b.IsAwake then
        sleeping := false;

    end;
    b := b.GetNext;
  end;

  if (m_stepCount = 180) then
    m_stepCount := m_stepCount + 0;

  //if (sleeping)
  //{
  //	CreateCircle();
  //}

  inherited Step(settings);

  b := m_world.GetBodyList;

  while (b.FHandle <> 0) do
  begin
    if (b.GetType = b2_dynamicBody) then
    begin
      p := b.GetPosition;
      if (p.x <= -10.0) or (10.0 <= p.x) or (p.y <= 0.0) or (20.0 <= p.y) then
        p.x := p.x + 0.0;
    end;
    b := b.GetNext;
  end;

  g_debugDraw.DrawString(5, m_textLine, 'Press ''c'' to create a circle.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Confined', @TConfined.CreateTest));
end.

