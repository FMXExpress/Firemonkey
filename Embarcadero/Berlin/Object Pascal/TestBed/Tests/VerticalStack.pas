//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit VerticalStack;

interface

uses
  Box2D.Dynamics, Test;

type
  TVerticalStack = class(TTest)
  protected const
		e_columnCount = 1;
		e_rowCount = 15;

  protected
    m_bullet: b2BodyWrapper;
    m_bodies: array [0..e_rowCount * e_columnCount -1] of b2BodyWrapper;
    m_indices: array [0..e_rowCount * e_columnCount -1] of Integer;
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

{ TVerticalStack }

constructor TVerticalStack.Create;
const
  xs: array [0..4] of Float32 = (0.0, -10.0, -5.0, 5.0, 10.0);
var
  I, J, N: Integer;
  bd: b2BodyDef;
  ground, body: b2BodyWrapper;
  edge: b2EdgeShapeWrapper;
  shape: b2PolygonShapeWrapper;
  fd: b2FixtureDef;
  x: Float32;
begin
  inherited Create;

  bd := b2BodyDef.Create;
  ground := m_world.CreateBody(@bd);

  edge := b2EdgeShapeWrapper.Create;
  edge.&Set(b2Vec2.Create(-40.0, 0.0), b2Vec2.Create(40.0, 0.0));
  ground.CreateFixture(edge, 0.0);

  edge.&Set(b2Vec2.Create(20.0, 0.0), b2Vec2.Create(20.0, 20.0));
  ground.CreateFixture(edge, 0.0);
  edge.Destroy;


  for J := 0 to e_columnCount - 1 do
  begin
    shape := b2PolygonShapeWrapper.Create;
    shape.SetAsBox(0.5, 0.5);

    fd := b2FixtureDef.Create;
    fd.shape := shape;
    fd.density := 1.0;
    fd.friction := 0.3;

    for I := 0 to e_rowCount - 1 do
    begin
      bd := b2BodyDef.Create;
      bd.&type := b2_dynamicBody;

      n := j * e_rowCount + i;
      //b2Assert(n < e_rowCount * e_columnCount);
      m_indices[n] := n;
      bd.userData := @m_indices[N];

      x := 0.0;
      //float32 x := RandomFloat(-0.02, 0.02);
      //float32 x := i % 2 == 0 ? -0.01 : 0.01;
      bd.position.&Set(xs[j] + x, 0.55 + 1.1 * i);
      body := m_world.CreateBody(@bd);

      m_bodies[n] := body;

      body.CreateFixture(@fd);
    end;
  end;

  m_bullet.FHandle := 0;

end;

class function TVerticalStack.CreateTest: TTest;
begin
  Result := TVerticalStack.Create;
end;

procedure TVerticalStack.Keyboard(key: Integer);
var
  shape: b2CircleShapeWrapper;
  fd: b2FixtureDef;
  bd: b2BodyDef;

begin
  case key of
    Ord(','):
    begin
      if (m_bullet.FHandle <> 0) then
      begin
        m_world.DestroyBody(m_bullet);
        m_bullet.FHandle := 0;
      end;

      shape := b2CircleShapeWrapper.Create;
      shape.m_radius := 0.25;

      fd := b2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.restitution := 0.05;

      bd := b2BodyDef.Create;
      bd.&type := b2_dynamicBody;
      bd.bullet := true;
      bd.position.&Set(-31.0, 5.0);

      m_bullet := m_world.CreateBody(@bd);
      m_bullet.CreateFixture(@fd);

      m_bullet.SetLinearVelocity(b2Vec2.Create(400.0, 0.0));

    end;

    Ord('B'), Ord('b'):
    begin
      // TODO: Umcomment this when global is available.
      //g_blockSolve := not g_blockSolve;
    end;

  end;
end;

procedure TVerticalStack.Step(settings: PSettings);
begin
  inherited;

end;

initialization
  RegisterTest(TestEntry.Create('VerticalStack', @TVerticalStack.CreateTest));
end.
