//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit BulletTest;

interface

uses
  Box2D.Dynamics, Test;

type
  TBulletTest = class(TTest)
  protected
    m_body: b2BodyWrapper;
    m_bullet: b2BodyWrapper;
    m_x: Float32;
  public
    constructor Create;
    class function CreateTest: TTest; static;

    procedure Launch;

    procedure Step(settings: PSettings); override;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, DebugDraw;


constructor TBulletTest.Create;
var
  bd: b2BodyDef;
  body: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  shape: b2PolygonShapeWrapper;
  box: b2PolygonShapeWrapper;

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


  bd := b2BodyDef.Create;
  bd.&type := b2_dynamicBody;
  bd.position.&Set(0.0, 4.0);

  box := b2PolygonShapeWrapper.Create;
  box.SetAsBox(2.0, 0.1);

  m_body := m_world.CreateBody(@bd);
  m_body.CreateFixture(box, 1.0);

  box.SetAsBox(0.25, 0.25);

  //m_x := RandomFloat(-1.0, 1.0);
  m_x := 0.20352793;
  bd.position.&Set(m_x, 10.0);
  bd.bullet := true;

  m_bullet := m_world.CreateBody(@bd);
  m_bullet.CreateFixture(box, 100.0);

  m_bullet.SetLinearVelocity(b2Vec2.Create(0.0, -50.0));
end;

class function TBulletTest.CreateTest: TTest;
begin
  Result := TBulletTest.Create;
end;

procedure TBulletTest.Launch;
begin
  m_body.SetTransform(b2Vec2.Create(0.0, 4.0), 0.0);
  //m_body.SetLinearVelocity(b2Vec2_zero);
  m_body.SetLinearVelocity(b2Vec2.Create(0.0, 0.0));
  m_body.SetAngularVelocity(0.0);

  m_x := RandomFloat(-1.0, 1.0);
  m_bullet.SetTransform(b2Vec2.Create(m_x, 10.0), 0.0);
  m_bullet.SetLinearVelocity(b2Vec2.Create(0.0, -50.0));
  m_bullet.SetAngularVelocity(0.0);

//  extern int32 b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters;
//  extern int32 b2_toiCalls, b2_toiIters, b2_toiMaxIters;
//  extern int32 b2_toiRootIters, b2_toiMaxRootIters;

  // TODO : uncomment when external variables are accessible

//  b2_gjkCalls := 0;
//  b2_gjkIters := 0;
//  b2_gjkMaxIters := 0;
//
//  b2_toiCalls := 0;
//  b2_toiIters := 0;
//  b2_toiMaxIters := 0;
//  b2_toiRootIters := 0;
//  b2_toiMaxRootIters := 0;


end;

procedure TBulletTest.Step(settings: PSettings);
begin
  inherited Step(settings);
//  extern int32 b2_gjkCalls, b2_gjkIters, b2_gjkMaxIters;
//  extern int32 b2_toiCalls, b2_toiIters;
//  extern int32 b2_toiRootIters, b2_toiMaxRootIters;

  // TODO : uncomment when external variables are accessible
//  if (b2_gjkCalls > 0) then
//  begin
//    g_debugDraw.DrawString(5, m_textLine, 'gjk calls = %d, ave gjk iters = %3.1f, max gjk iters = %d',
//      [b2_gjkCalls, b2_gjkIters / float32(b2_gjkCalls), b2_gjkMaxIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//  end;
//
//  if (b2_toiCalls > 0) then
//  begin
//    g_debugDraw.DrawString(5, m_textLine, 'toi calls = %d, ave toi iters = %3.1f, max toi iters = %d',
//      [b2_toiCalls, b2_toiIters / float32(b2_toiCalls), b2_toiMaxRootIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//
//    g_debugDraw.DrawString(5, m_textLine, 'ave toi root iters = %3.1f, max toi root iters = %d',
//      [b2_toiRootIters / float32(b2_toiCalls), b2_toiMaxRootIters]);
//    m_textLine := m_textLine + DRAW_STRING_NEW_LINE;
//  end;

  if (m_stepCount mod 60) = 0 then
  begin
    Launch;
  end;
end;

initialization
  RegisterTest(TestEntry.Create('BulletTest', @TBulletTest.CreateTest));
end.

