
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uSimulationFmxCtrls;

interface

uses
  System.Types, FMX.Controls,
  Box2D.Common, Box2D.Collision, Box2D.Dynamics,
  uFlatBox2DSimulation;

type
  TSimulationFmxCtrls = class(TFlatBox2DSimulation)
  public
    procedure AddBoundaryBox(posX, posY, halfSizeX, halfSizeY: Single);
    function AddDynamicBox(posX, posY, halfSizeX, halfSizeY: Single; ctrl: TControl;
      angle: Single = 0; friction: Single = 0.7; restitution: Single = 0.5): b2BodyHandle;
  end;

implementation

{ TSimulationBoxes }

procedure TSimulationFmxCtrls.AddBoundaryBox(posX, posY, halfSizeX,
  halfSizeY: Single);
var
  boundaryBodyDef: b2BodyDef;
  boundaryBody: b2BodyWrapper;
  boundaryBox: b2PolygonShapeWrapper;
begin
  boundaryBodyDef := b2BodyDef.Create();
  boundaryBodyDef.position.&Set(posX, posY);
  boundaryBody := world.CreateBody(@boundaryBodyDef);

  boundaryBox := b2PolygonShapeWrapper.Create();
  boundaryBox.SetAsBox(halfSizeX, halfSizeY);
  boundaryBody.CreateFixture(boundaryBox, 0);
  boundaryBox.Destroy;
end;

function TSimulationFmxCtrls.AddDynamicBox(posX, posY, halfSizeX, halfSizeY: Single;
  ctrl: TControl; angle, friction, restitution: Single): b2BodyHandle;
var
  bodyDef: b2BodyDef;
  body: b2BodyWrapper;
  dynamicBox: b2PolygonShapeWrapper;
  fixtureDef: b2FixtureDef;
begin
  bodyDef := b2BodyDef.Create();
  bodyDef.&type := b2_DynamicBody;
  bodyDef.position.&Set(posX, posY);
  bodyDef.angle := angle;

  body := world.CreateBody(@bodyDef);
  dynamicBox := b2PolygonShapeWrapper.Create();
  dynamicBox.SetAsBox(halfSizeX, halfSizeY);
  fixtureDef := b2FixtureDef.Create();
  fixtureDef.shape := dynamicBox;
  fixtureDef.density := 1.0;
  fixtureDef.friction := friction;
  fixtureDef.restitution := restitution;
  body.CreateFixture(@fixtureDef);
  dynamicBox.Destroy;

  body.SetUserData(ctrl);
  Result := body;
end;

end.
