unit uFlatBox2DSimulation;

interface

uses
  uCustomSimulation, Box2D.Common, Box2D.Collision, Box2D.Dynamics;

type
  TFlatBox2DSimulation = class(TCustomSimulation)
  private
    FPositionIterations: integer;
    FVelocityIterations: integer;
    procedure SetGravity(const Value: b2Vec2);
    procedure SetPositionIterations(const Value: integer);
    procedure SetVelocityIterations(const Value: integer);
  protected
    FGravity: b2Vec2;
    FWorld: b2WorldWrapper;
    procedure Initialize; override;
    procedure Step(deltaTime: Double = 1/60); override;
    procedure Finalize; override;
  public
    property Gravity: b2Vec2 read FGravity write SetGravity;
    property World: b2WorldWrapper read FWorld;
    property VelocityIterations: integer read FVelocityIterations write SetVelocityIterations;
    property PositionIterations: integer read FPositionIterations write SetPositionIterations;
  end;

  TFlatBox2DSimulationClass = class of TFlatBox2DSimulation;

implementation

{ TFlatBox2DSimulation }

procedure TFlatBox2DSimulation.Initialize;
begin
  FVelocityIterations := 6;
  FPositionIterations := 2;
  FGravity := b2Vec2.Create(0.0, -10.0);
  FWorld := b2WorldWrapper.Create(Gravity);
end;

procedure TFlatBox2DSimulation.Finalize;
begin
  FWorld.Destroy;
end;

procedure TFlatBox2DSimulation.SetGravity(const Value: b2Vec2);
begin
  FGravity := Value;
  FWorld.SetGravity(Value);
end;

procedure TFlatBox2DSimulation.SetPositionIterations(const Value: integer);
begin
  FPositionIterations := Value;
end;

procedure TFlatBox2DSimulation.SetVelocityIterations(const Value: integer);
begin
  FVelocityIterations := Value;
end;

procedure TFlatBox2DSimulation.Step(deltaTime: Double);
begin
  FWorld.Step(deltaTime, FVelocityIterations, FPositionIterations);
end;

end.
