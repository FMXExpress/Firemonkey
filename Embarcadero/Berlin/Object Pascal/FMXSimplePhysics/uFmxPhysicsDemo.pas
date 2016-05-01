unit uFmxPhysicsDemo;

interface

uses
  System.Types, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Graphics,
  uSimulationFmxCtrls;

type
  TFmxPhysicsDemo = class
  private
    FCtrls: TList<TControl>;
    FTimer: TTimer;
    FSimCtrls: TSimulationFmxCtrls;
    FOffsetX: Double;
    FOffsetY: Double;
    FScaleX: Double;
    FScaleY: Double;
    FScreenWidth, FScreenHeight: Double;
    procedure SetIsRunning(const Value: boolean);
    procedure DoOnTimer(Sender: TObject);
    function GetIsRunning: boolean;
    procedure CtrlToBox(ctrlPos: TPointF; ctrlWidth, ctrlHeight: Single;
      var worldPosX, worldPosY, worldHalfWidth, worldHalfHeight: Single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddControl(ctrl: TControl);
    procedure AddScreenSize(width, height: Double);
    procedure Start;
    procedure Stop;
    property IsRunning: boolean read GetIsRunning write SetIsRunning;
  end;

implementation

uses
  Box2D.Common, Box2D.Dynamics, uFmxControlHelper;

{ TFmxPhysicsDemo }

constructor TFmxPhysicsDemo.Create;
begin
  FScaleX := 10;
  FScaleY := 10;
  FOffsetX:= 0;
  FOffsetY:= 0;
  FScreenWidth := 640; // default form width
  FScreenHeight := 480; // default form height
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 15;
  FTimer.OnTimer := DoOnTimer;
  FSimCtrls := TSimulationFmxCtrls.Create;
  FCtrls := TList<TControl>.Create;
end;

destructor TFmxPhysicsDemo.Destroy;
begin
  Stop;
  FCtrls.Free;
  FSimCtrls.Free;
  FTimer.Free;
  inherited;
end;

procedure TFmxPhysicsDemo.DoOnTimer(Sender: TObject);
var i: integer; ctrl: TControl; body: b2BodyWrapper;
  bodyPos: b2Vec2; angle: Single; ctrlCentre: TPointF;
begin
  FSimCtrls.DoStep;

  for i := 0 to FCtrls.Count-1 do
  begin
    ctrl := FCtrls.Items[i];
    body.FHandle := ctrl.Tag;
    bodyPos := body.GetPosition^;
    angle := body.GetAngle;

    ctrlCentre.X := bodyPos.x * FScaleX + FScreenWidth/2;
    ctrlCentre.Y := FScreenHeight/2 - bodyPos.y * FScaleY;

    ctrl.Position.X := ctrlCentre.X - ctrl.Width/2;
    ctrl.Position.Y := ctrlCentre.Y - ctrl.Height/2;

    ctrl.SetRotation(-angle * 180 / pi);
  end;

end;

function TFmxPhysicsDemo.GetIsRunning: boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TFmxPhysicsDemo.SetIsRunning(const Value: boolean);
begin
  if Value <> IsRunning then
    FTimer.Enabled := Value;
end;

procedure TFmxPhysicsDemo.Start;
begin
  IsRunning := True;
end;

procedure TFmxPhysicsDemo.Stop;
begin
  IsRunning := False;
end;

procedure TFmxPhysicsDemo.AddScreenSize(width, height: Double);
const ht = 0.5; // half of boudary box thickness, in world scale
var hw, hh: Double;
begin
  FScreenWidth := width;
  FScreenHeight := height;

  hw := width / FScaleX / 2;
  hh := height / FScaleY / 2;

  FSimCtrls.AddBoundaryBox(0, -hh-ht, hw+ht, ht); // bottom
  FSimCtrls.AddBoundaryBox(0, hh+ht, hw+ht, ht); // top
  FSimCtrls.AddBoundaryBox(-hw-ht, 0, ht, hh); // left
  FSimCtrls.AddBoundaryBox(hw+ht, 0, ht, hh); // right
end;

procedure TFmxPhysicsDemo.AddControl(ctrl: TControl);
var x,y,hw,hh: Single; handle: b2BodyHandle;
begin
  FCtrls.Add(ctrl);
  CtrlToBox(ctrl.Position.Point, ctrl.Width, ctrl.Height, x, y, hw, hh);
  handle := FSimCtrls.AddDynamicBox(x, y, hw, hh, ctrl);
  ctrl.Tag := handle;
end;

procedure TFmxPhysicsDemo.CtrlToBox(ctrlPos: TPointF; ctrlWidth,
  ctrlHeight: Single; var worldPosX, worldPosY, worldHalfWidth,
  worldHalfHeight: Single);
var ctrlCentre: TPointF;
begin
  worldHalfWidth := ctrlWidth / FScaleX / 2;
  worldHalfHeight := ctrlHeight / FScaleY / 2;

  ctrlCentre.X := ctrlPos.X + ctrlWidth / 2;
  ctrlCentre.Y := ctrlPos.Y + ctrlHeight / 2;

  worldPosX := (ctrlCentre.X - FScreenWidth/2) / FScaleX;
  worldPosY := (-ctrlCentre.Y + FScreenHeight/2) / FScaleY;
end;

end.
