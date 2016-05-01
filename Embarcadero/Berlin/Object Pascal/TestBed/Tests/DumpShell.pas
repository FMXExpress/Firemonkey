//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit DumpShell;

interface

uses
  Test;

type
  TDumpShell = class(TTest)
  protected
  public
    constructor Create;
    class function CreateTest: TTest; static;
end;

implementation

uses
  System.Math,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics, DebugDraw;


constructor TDumpShell.Create;
var
  g: b2Vec2;
  bodies: array [0..2] of b2BodyWrapper;
  bd: b2BodyDef;
  fd: b2FixtureDef;
  shape: b2PolygonShapeWrapper;
  vs: array [0..7] of b2Vec2;

begin
  inherited Create;

  //Source code dump of Box2D scene: issue304-minimal-case.rube
  //
  //  Created by R.U.B.E 1.3.0
  //  Using Box2D version 2.3.0
  //  Wed April 3 2013 04:33:28
  //
  //  This code is originally intended for use in the Box2D testbed,
  //  but you can easily use it in other applications by providing
  //  a b2World for use as the 'm_world' variable in the code below.

  g := b2Vec2.Create(0.000000000000000e+00, -1.000000000000000e+01);
  m_world.SetGravity(g);

  //b2Body** bodies = (b2Body** )b2Alloc(3 * sizeof(b2Body* ));
  //b2Joint** joints = (b2Joint** )b2Alloc(0 * sizeof(b2Joint* ));

  bd := b2BodyDef.Create;
  bd.&type := b2BodyType(0);
  bd.position.&Set(2.587699890136719e-02, 5.515012264251709e+00);
  bd.angle := 0.000000000000000e+00;
  bd.linearVelocity.&Set(0.000000000000000e+00, 0.000000000000000e+00);
  bd.angularVelocity := 0.000000000000000e+00;
  bd.linearDamping := 0.000000000000000e+00;
  bd.angularDamping := 0.000000000000000e+00;
  bd.allowSleep := True;//bool(4);
  bd.awake := True;//bool(2);
  bd.fixedRotation := False;//bool(0);
  bd.bullet := False;//bool(0);
  bd.active := True;//bool(32);
  bd.gravityScale := 1.000000000000000e+00;
  bodies[0] := m_world.CreateBody(@bd);

  fd := b2FixtureDef.Create;
  fd.friction := 2.000000029802322e-01;
  fd.restitution := 0.000000000000000e+00;
  fd.density := 1.000000000000000e+00;
  fd.isSensor := False;//bool(0);
  fd.filter.categoryBits := uint16(1);
  fd.filter.maskBits := uint16(65535);
  fd.filter.groupIndex := int16(0);

  shape := b2PolygonShapeWrapper.Create;

  vs[0].&Set(7.733039855957031e-01, -1.497260034084320e-01);
  vs[1].&Set(-4.487270116806030e-01, 1.138330027461052e-01);
  vs[2].&Set(-1.880589962005615e+00, -1.365900039672852e-01);
  vs[3].&Set(3.972740173339844e-01, -3.897832870483398e+00);
  shape.&Set(@vs[0], 4);

  fd.shape := shape;

  bodies[0].CreateFixture(@fd);

  shape.Destroy;


  bd := b2BodyDef.Create;
  bd.&type := b2BodyType(2);
  bd.position.&Set(-3.122138977050781e-02, 7.535382270812988e+00);
  bd.angle := -1.313644275069237e-02;
  bd.linearVelocity.&Set(8.230687379837036e-01, 7.775862514972687e-02);
  bd.angularVelocity := 3.705333173274994e-02;
  bd.linearDamping := 0.000000000000000e+00;
  bd.angularDamping := 0.000000000000000e+00;
  bd.allowSleep := True;//bool(4);
  bd.awake := True;//bool(2);
  bd.fixedRotation := False;//bool(0);
  bd.bullet := False;//bool(0);
  bd.active := True;//bool(32);
  bd.gravityScale := 1.000000000000000e+00;
  bodies[1] := m_world.CreateBody(@bd);

  fd := b2FixtureDef.Create;
  fd.friction := 5.000000000000000e-01;
  fd.restitution := 0.000000000000000e+00;
  fd.density := 5.000000000000000e+00;
  fd.isSensor := False;//bool(0);
  fd.filter.categoryBits := uint16(1);
  fd.filter.maskBits := uint16(65535);
  fd.filter.groupIndex := int16(0);
  shape := b2PolygonShapeWrapper.Create;
  vs[0].&Set(3.473900079727173e+00, -2.009889930486679e-01);
  vs[1].&Set(3.457079887390137e+00, 3.694039955735207e-02);
  vs[2].&Set(-3.116359949111938e+00, 2.348500071093440e-03);
  vs[3].&Set(-3.109960079193115e+00, -3.581250011920929e-01);
  vs[4].&Set(-2.590820074081421e+00, -5.472509860992432e-01);
  vs[5].&Set(2.819370031356812e+00, -5.402340292930603e-01);
  shape.&Set(@vs[0], 6);

  fd.shape := shape;

  bodies[1].CreateFixture(@fd);
  shape.Destroy;

  //
  bd := b2BodyDef.Create;
  bd.&type := b2BodyType(2);
  bd.position.&Set(-7.438077926635742e-01, 6.626811981201172e+00);
  bd.angle := -1.884713363647461e+01;
  bd.linearVelocity.&Set(1.785794943571091e-01, 3.799796104431152e-07);
  bd.angularVelocity := -5.908820639888290e-06;
  bd.linearDamping := 0.000000000000000e+00;
  bd.angularDamping := 0.000000000000000e+00;
  bd.allowSleep := True;//bool(4);
  bd.awake := True;//bool(2);
  bd.fixedRotation := False;//bool(0);
  bd.bullet := False;//bool(0);
  bd.active := True;//bool(32);
  bd.gravityScale := 1.000000000000000e+00;
  bodies[2] := m_world.CreateBody(@bd);


  fd := b2FixtureDef.Create;
  fd.friction := 9.499999880790710e-01;
  fd.restitution := 0.000000000000000e+00;
  fd.density := 1.000000000000000e+01;
  fd.isSensor := False;//bool(0);
  fd.filter.categoryBits := uint16(1);
  fd.filter.maskBits := uint16(65535);
  fd.filter.groupIndex := int16(-3);
  shape := b2PolygonShapeWrapper.Create;
  vs[0].&Set(1.639146506786346e-01, 4.428443685173988e-02);
  vs[1].&Set(-1.639146655797958e-01, 4.428443685173988e-02);
  vs[2].&Set(-1.639146655797958e-01, -4.428443312644958e-02);
  vs[3].&Set(1.639146357774734e-01, -4.428444057703018e-02);
  shape.&Set(@vs[0], 4);

  fd.shape := shape;

  bodies[2].CreateFixture(@fd);
  shape.Destroy;
end;

class function TDumpShell.CreateTest: TTest;
begin
  Result := TDumpShell.Create;
end;

initialization
  RegisterTest(TestEntry.Create('DumpShell', @TDumpShell.CreateTest));
end.

