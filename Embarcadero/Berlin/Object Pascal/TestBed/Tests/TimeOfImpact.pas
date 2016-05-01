//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TimeOfImpact;

interface

uses
  Box2D.Collision, Test;

type
  TTimeOfImpact = class(TTest)
  protected
    m_shapeA, m_shapeB: b2PolygonShapeWrapper;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateTest: TTest; static;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Dynamics, DebugDraw;

{ TTimeOfImpact }

constructor TTimeOfImpact.Create;
begin
  inherited Create;
  m_shapeA := b2PolygonShapeWrapper.Create;
  m_shapeA.SetAsBox(25.0, 5.0);
  m_shapeB := b2PolygonShapeWrapper.Create;
  m_shapeB.SetAsBox(2.5, 2.5);
end;

class function TTimeOfImpact.CreateTest: TTest;
begin
  Result := TTimeOfImpact.Create;
end;

destructor TTimeOfImpact.Destroy;
begin
  m_shapeB.Destroy;
  m_shapeA.Destroy;
end;

procedure TTimeOfImpact.Step(settings: PSettings);
var
  sweepA, sweepB: b2Sweep;
  input: b2TOIInput;
  output: b2TOIOutput;
  vertices: array [0..b2_maxPolygonVertices-1] of b2Vec2;
  transformA, transformB: b2Transform;
  I: Integer;
  t: Float32;
begin
  inherited Step(settings);

  sweepA := b2Sweep.Create;
  sweepA.c0.&Set(24.0, -60.0);
  sweepA.a0 := 2.95;
  sweepA.c := sweepA.c0;
  sweepA.a := sweepA.a0;
  sweepA.localCenter.SetZero();

  sweepB := b2Sweep.Create;
  sweepB.c0.&Set(53.474274, -50.252514);
  sweepB.a0 := 513.36676; // - 162.0 * b2_pi;
  sweepB.c.&Set(54.595478, -51.083473);
  sweepB.a := 513.62781; //  - 162.0 * b2_pi;
  sweepB.localCenter.SetZero();

  //sweepB.a0 -= 300.0 * b2_pi;
  //sweepB.a -= 300.0 * b2_pi;

  input := b2TOIInput.Create;
  input.proxyA.&Set(m_shapeA, 0);
  input.proxyB.&Set(m_shapeB, 0);
  input.sweepA := sweepA;
  input.sweepB := sweepB;
  input.tMax := 1.0;

  output := b2TOIOutput.Create;

  b2TimeOfImpact(@output, @input);

  g_debugDraw.DrawString(5, m_textLine, 'toi := %g', [output.t]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;

//  extern int32 b2_toiMaxIters, b2_toiMaxRootIters;
  // TODO: Uncomment when globals are available.
  //g_debugDraw.DrawString(5, m_textLine, 'max toi iters := %d, max root iters := %d', [b2_toiMaxIters, b2_toiMaxRootIters]);
  m_textLine := m_textLine + DRAW_STRING_NEW_LINE;


  transformA := b2Transform.Create;
  sweepA.GetTransform(@transformA, 0.0);
  for I := 0 to m_shapeA.m_count - 1 do
  begin
    vertices[i] := b2Mul(transformA, m_shapeA.m_vertices[i]);
  end;

  g_debugDraw.DrawPolygon(@vertices[0], m_shapeA.m_count, b2Color.Create(0.9, 0.9, 0.9, 1.0));

  transformA := b2Transform.Create;
  sweepB.GetTransform(@transformB, 0.0);

  for I := 0 to m_shapeB.m_count - 1 do
  begin
    vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
  end;
  g_debugDraw.DrawPolygon(@vertices[0], m_shapeB.m_count, b2Color.Create(0.5, 0.9, 0.5, 1.0));

  sweepB.GetTransform(@transformB, output.t);
  for I := 0 to m_shapeB.m_count - 1 do
  begin
    vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
  end;
  g_debugDraw.DrawPolygon(@vertices[0], m_shapeB.m_count, b2Color.Create(0.5, 0.7, 0.9, 1.0));

  sweepB.GetTransform(@transformB, 1.0);
  for I := 0 to m_shapeB.m_count - 1 do
  begin
    vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
  end;
  g_debugDraw.DrawPolygon(@vertices[0], m_shapeB.m_count, b2Color.Create(0.9, 0.5, 0.5, 1.0));

{$IFDEF OPTIONAL}
  t := 0.0;
  while (t < 1.0) do
  begin
    sweepB.GetTransform(@transformB, t);
    for I := 0 to m_shapeB.m_count - 1 do
    begin
      vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
    end;
    g_debugDraw.DrawPolygon(@vertices[0], m_shapeB.m_count, b2Color.Create(0.9, 0.5, 0.5, 1.0));
    t := t + 0.1;
  end;
{$ENDIF OPTIONAL}

end;

initialization
  RegisterTest(TestEntry.Create('TimeOfImpact', @TTimeOfImpact.CreateTest));
end.
