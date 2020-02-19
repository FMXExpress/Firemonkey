//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ContinuousTest;

interface

uses
  Box2D.Dynamics, Test;

type
  TContinuousTest = class(TTest)
  protected
    m_body: b2BodyWrapper;
    m_angularVelocity: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Launch;

    procedure Step(settings: PSettings); override;
    procedure Keyboard(key: Integer); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;

{$DEFINE OPTION1}

constructor TContinuousTest.Create;
var
  bd: b2BodyDef;
  body: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  shape: b2PolygonShapeWrapper;
{$IF not defined(OPTION1)}
  circle: b2CircleShapeWrapper;
{$ENDIF}

begin
  inherited Create;

  bd := b2BodyDef.Create;
  bd.position.&Set(0.0, 0.0);
  body := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;

  edge.&Set(b2Vec2.Create(-10.0, 0.0), b2Vec2.Create(10.0, 0.0));
  body.CreateFixture(edge, 0.0);

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(0.2, 1.0, b2Vec2.Create(0.5, 1.0), 0.0);
  body.CreateFixture(shape, 0.0);
  edge.Destroy;
  shape.Destroy;

{$IF defined(OPTION1)}
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 20.0);
  //bd.angle := 0.1f;

  shape := b2PolygonShapeWrapper.Create;
  shape.SetAsBox(2.0, 0.1);

  m_body := m_world.CreateBody(@bd);
  m_body.CreateFixture(shape, 1.0);

  m_angularVelocity := RandomFloat(-50.0, 50.0);
  //m_angularVelocity := 46.661274f;
  m_body.SetLinearVelocity(b2Vec2.Create(0.0, -100.0));
  m_body.SetAngularVelocity(m_angularVelocity);
{$ELSE }
  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 2.0);
  body := m_world.CreateBody(@bd);

  circle := b2CircleShapeWrapper.Create;
  circle.Set_m_radius(b2Vec2.Create(0.0, 0.0));
  circle.Set_m_radius(0.5);
  body.CreateFixture(circle, 1.0);

  bd.bullet := true;
  bd.position.Set(0.0, 10.0);
  body := m_world.CreateBody(@bd);
  body.CreateFixture(circle, 1.0);
  body.SetLinearVelocity(b2Vec2.Create(0.0, -100.0));
  circle.Destroy;
{$ENDIF}

//		extern int32 b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters;
//		extern int32 b2_toiCalls, b2_toiIters;
//		extern int32 b2_toiRootIters, b2_toiMaxRootIters;
//		extern float32 b2_toiTime, b2_toiMaxTime;

//		b2_gjkCalls := 0; b2_gjkIters := 0; b2_gjkMaxIters := 0;
//		b2_toiCalls := 0; b2_toiIters := 0;
//		b2_toiRootIters := 0; b2_toiMaxRootIters := 0;
//		b2_toiTime := 0.0; b2_toiMaxTime := 0.0;
end;

class function TContinuousTest.CreateTest: TTest;
begin
  Result := TContinuousTest.Create;
end;

procedure TContinuousTest.Keyboard(key: Integer);
begin
  inherited;

end;

procedure TContinuousTest.Launch;
begin
//  extern int32 b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters;
//  extern int32 b2_toiCalls, b2_toiIters;
//  extern int32 b2_toiRootIters, b2_toiMaxRootIters;
//  extern float32 b2_toiTime, b2_toiMaxTime;

//  b2_gjkCalls := 0; b2_gjkIters := 0; b2_gjkMaxIters := 0;
//  b2_toiCalls := 0; b2_toiIters := 0;
//  b2_toiRootIters := 0; b2_toiMaxRootIters := 0;
//  b2_toiTime := 0.0; b2_toiMaxTime := 0.0;

  m_body.SetTransform(b2Vec2.Create(0.0, 20.0), 0.0);
  m_angularVelocity := RandomFloat(-50.0, 50.0);
  m_body.SetLinearVelocity(b2Vec2.Create(0.0, -100.0));
  m_body.SetAngularVelocity(m_angularVelocity);
end;

procedure TContinuousTest.Step(settings: PSettings);
begin
  inherited Step(settings);
//  extern int32 b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters;

//  if (b2_gjkCalls > 0) then
//  begin
//    g_debugDraw.DrawString(5, m_textLine, 'gjk calls = %d, ave gjk iters = %3.1f, max gjk iters = %d',
//      [b2_gjkCalls, b2_gjkIters / b2_gjkCalls, b2_gjkMaxIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//  end;

//  extern int32 b2_toiCalls, b2_toiIters;
//  extern int32 b2_toiRootIters, b2_toiMaxRootIters;
//  extern float32 b2_toiTime, b2_toiMaxTime;

//  if (b2_toiCalls > 0) then
//  begin
//    g_debugDraw.DrawString(5, m_textLine, 'toi calls = %d, ave [max] toi iters = %3.1f [%d]',
//      [b2_toiCalls, b2_toiIters / b2_toiCalls, b2_toiMaxRootIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//
//    g_debugDraw.DrawString(5, m_textLine, 'ave [max] toi root iters = %3.1f [%d]',
//      [b2_toiRootIters / b2_toiCalls, b2_toiMaxRootIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//
//    g_debugDraw.DrawString(5, m_textLine, 'ave [max] toi time = %.1f [%.1f] (microseconds)',
//      [1000.0 * b2_toiTime / b2_toiCalls, 1000.0 * b2_toiMaxTime]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//  end;

  if (m_stepCount mod 60) = 0 then
  begin
    //Launch();
  end;
end;

initialization
  RegisterTest(TestEntry.Create('ContinuousTest', @TContinuousTest.CreateTest));
end.

