//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ConvexHull;

interface

uses
   Box2D.Common, Test;

type
  TConvexHull = class(TTest)
  protected
  const
    // TODO : Delphi cannot use values that are in #define like "#define b2_maxPolygonVertices 8;
    e_count = 8; //b2_maxPolygonVertices;
  protected var
    m_count: Int32;
    m_auto: Boolean;
    m_points: array [0..e_count-1] of b2Vec2;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Generate;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
end;

implementation

uses
  System.Math,
 Box2D.Collision, Box2D.Dynamics, DebugDraw;


{ TConvexHull }

constructor TConvexHull.Create;
begin
  inherited Create;
  Generate;
  m_auto := false;
end;

class function TConvexHull.CreateTest: TTest;
begin
  Result := TConvexHull.Create;
end;

procedure TConvexHull.Generate;
var
  lowerBound, upperBound, v: b2Vec2;
  I: Integer;
  x, y: Float32;
begin
  lowerBound := b2Vec2.Create(-8.0, -8.0);
  upperBound := b2Vec2.Create(8.0, 8.0);

  for I := 0 to e_count - 1 do
  begin
    x := 10.0 * RandomFloat();
    y := 10.0 * RandomFloat();

    // Clamp onto a square to help create collinearities.
    // This will stress the convex hull algorithm.
    v := b2Vec2.Create(x, y);
    v := b2Clamp(v, lowerBound, upperBound);
    m_points[I] := v;
  end;

  m_count := e_count;
end;

procedure TConvexHull.Keyboard(key: Integer);
begin
  case key of
  Ord('A'), Ord('a'):
    m_auto :=  not m_auto;

  Ord('G'), Ord('g'):
    Generate();
  end;
end;

procedure TConvexHull.Step(settings: PSettings);
var
  I: Integer;
  shape: b2PolygonShapeWrapper;

begin
  inherited Step(settings);

  shape := b2PolygonShapeWrapper.Create;
  shape.&Set(@m_points[0], m_count);

  g_debugDraw.DrawString(5, m_textLine, 'Press g to generate a new random convex hull');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

  // TODO : Enable line when m_vertices field can be accessible...
  //g_debugDraw.DrawPolygon(shape.m_vertices, shape.m_count, b2Color(0.9f, 0.9f, 0.9f));

  for I := 0 to m_count - 1 do
  begin
    g_debugDraw.DrawPoint(m_points[i], 3.0, b2Color.Create(0.3, 0.9, 0.3, 1.0));
    g_debugDraw.DrawString(m_points[i] + b2Vec2.Create(0.05, 0.05), '%d', [I]);
  end;

  if shape.Validate = false then
    m_textLine := m_textLine + 0;

  if m_auto then
    Generate();

end;

initialization
  RegisterTest(TestEntry.Create('ConvexHull', @TConvexHull.CreateTest));
end.

