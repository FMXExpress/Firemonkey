//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Rope;

interface

uses
  Box2D.Rope, Test;

type
  TRope = class(TTest)
  protected
    m_rope: b2RopeWrapper;
    m_angle: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Keyboard(key: Integer); override;
    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;

{ TRope }

constructor TRope.Create;
const
  N = 40;
var
  I: Integer;
  vertices: array [0..N-1] of b2Vec2;
  masses: array [0..N-1] of Float32;
  def: b2RopeDef;
begin
  inherited Create;

  for I := 0 to N-1 do
  begin
    vertices[i].&Set(0.0, 20.0 - 0.25 * i);
    masses[i] := 1.0;
  end;
  masses[0] := 0.0;
  masses[1] := 0.0;

  def := b2RopeDef.Create;
  def.vertices := @vertices[0];
  def.count := N;
  def.gravity.&Set(0.0, -10.0);
  def.masses := @masses[0];
  def.damping := 0.1;
  def.k2 := 1.0;
  def.k3 := 0.5;

  m_rope := b2RopeWrapper.Create;
  m_rope.Initialize(@def);

  m_angle := 0.0;
  m_rope.SetAngle(m_angle);

end;

class function TRope.CreateTest: TTest;
begin
  Result := TRope.Create;
end;

procedure TRope.Keyboard(key: Integer);
begin
  case key of
    Ord('Q'), Ord('q'):
    begin
			m_angle := Max(-pi, m_angle - 0.05 * pi);
			m_rope.SetAngle(m_angle);
    end;

    Ord('E'), Ord('e'):
    begin
			m_angle := Min(pi, m_angle + 0.05 * pi);
			m_rope.SetAngle(m_angle);
    end;
  end;
end;

procedure TRope.Step(settings: PSettings);
var
  dt: Float32;
begin
  if settings.hz > 0.0 then
    dt := 1.0 / settings.hz
  else
    dt := 0.0;

  if (settings.pause = True) and (settings.singleStep = False) then
    dt := 0.0;

  m_rope.Step(dt, 1);

  inherited Step(settings);

  m_rope.Draw(g_debugDraw.DrawHandle);

  g_debugDraw.DrawString(5, m_textLine, 'Press (q,e) to adjust target angle');
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
  g_debugDraw.DrawString(5, m_textLine, 'Target angle = %g degrees', [m_angle * 180.0 / pi]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
end;

initialization
  RegisterTest(TestEntry.Create('Rope', @TRope.CreateTest));
end.

