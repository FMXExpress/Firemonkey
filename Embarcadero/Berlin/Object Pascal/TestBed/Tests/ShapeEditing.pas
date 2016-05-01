//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ShapeEditing;

interface

uses
  Box2D.Dynamics, Test;

type
  TShapeEditing = class(TTest)
  protected
    m_body: b2BodyWrapper;
    m_fixture1, m_fixture2: pb2Fixture;
    m_sensor: Boolean;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{ TShapeEditing }

constructor TShapeEditing.Create;
var
  ground: b2BodyWrapper;
  bd: b2BodyDef;
  edgeSahape: b2EdgeShapeWrapper;
  polyShape: b2PolygonShapeWrapper;

begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edgeSahape := b2EdgeShapeWrapper.Create;
  edgeSahape.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edgeSahape, 0.0);
  edgeSahape.Destroy;

  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 10.0);
  m_body := m_world.CreateBody(@bd);

  polyShape := b2PolygonShapeWrapper.Create;
  polyShape.SetAsBox(4.0, 4.0, b2Vec2.Create(0.0, 0.0), 0.0);
  m_fixture1 := m_body.CreateFixture(polyShape, 10.0);
  m_fixture2 := nil;
  polyShape.Destroy;

  m_sensor := False;
end;

class function TShapeEditing.CreateTest: TTest;
begin
  Result := TShapeEditing.Create;
end;

procedure TShapeEditing.Keyboard(key: Integer);
var
  circleShape: b2CircleShapeWrapper;
begin
  case key of
    Ord('C'), Ord('c'):
      if m_fixture2 = nil then
      begin
        circleShape := b2CircleShapeWrapper.Create;
        circleShape.m_radius := 3.0;
        circleShape.m_p := b2Vec2.Create(0.5, -4.0);
        m_fixture2 := m_body.CreateFixture(circleShape, 10.0);
        m_body.SetAwake(True);
        circleShape.Destroy;
      end;

    Ord('D'), Ord('d'):
      if m_fixture2 <> nil then
      begin
        m_body.DestroyFixture(m_fixture2);
        m_fixture2 := nil;
        m_body.SetAwake(True);
      end;

    Ord('S'), Ord('s'):
      if m_fixture2 <> nil then
      begin
        m_sensor := not m_sensor;
        m_fixture2.SetSensor(m_sensor);
      end;
  end;
end;

procedure TShapeEditing.Step(settings: PSettings);
begin
  inherited Step(settings);
  g_debugDraw.DrawString(5, m_textLine, 'Press: (c) create a shape, (d) destroy a shape.');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'sensor = %d', [Integer(m_sensor)]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('ShapeEditing', @TShapeEditing.CreateTest));
end.
