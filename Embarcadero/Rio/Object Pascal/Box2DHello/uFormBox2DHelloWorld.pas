//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uFormBox2DHelloWorld;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure Log(const msg: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Box2D.Common, Box2D.Collision, Box2D.Dynamics;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  gravity: b2Vec2;
  world: b2WorldWrapper;
  groundBodyDef: b2BodyDef;
  groundBody: b2BodyWrapper;
  groundBox: b2PolygonShapeWrapper;
  bodyDef: b2BodyDef;
  body: b2BodyWrapper;
  dynamicBox: b2PolygonShapeWrapper;
  fixtureDef: b2FixtureDef;
  timeStep: Double;
  velocityIterations: Integer;
  positionIterations: Integer;
  i: integer;
  pos: Pb2Vec2;
  angle: Double;
begin
  // Define the gravity vector
  gravity := b2Vec2.Create(0.0, -10.0);

  // Construct a world object, which will hold and simulate the rigid bodies
  world := b2WorldWrapper.Create(gravity);
  try
    // Define the ground body
    groundBodyDef := b2BodyDef.Create();
    groundBodyDef.position.&Set(0.0, -10.0);

    // Call the body factory which allocates memory for the ground body
    groundBody := world.CreateBody(@groundBodyDef);

    // Define the ground box shape
    groundBox := b2PolygonShapeWrapper.Create();

    // The extents are the half-widths of the box
    groundBox.SetAsBox(50.0, 10.0);

    // Add the ground fixture to the ground body
    groundBody.CreateFixture(groundBox, 0.0);

    // Define another box shape for our dynamic body
    bodyDef := b2BodyDef.Create();
    bodyDef.&type := b2_DynamicBody;
    bodyDef.position.&Set(0.0, 4.0);
    body := world.CreateBody(@bodyDef);

    dynamicBox := b2PolygonShapeWrapper.Create();
    dynamicBox.SetAsBox(1.0, 1.0);

    fixtureDef := b2FixtureDef.Create();
    fixtureDef.shape := dynamicBox;

    // Set the box density to be non-zero, so it will be dynamic
    fixtureDef.density := 1.0;

    // Override the default friction
    fixtureDef.friction := 1.0;

    // Add the shape to the body
    body.CreateFixture(@fixtureDef);

    // Cleanup Shape Wrappers
    dynamicBox.Destroy;
    groundBox.Destroy;

    // Prepare for simulation. Typically we use a time step of 1/60 of a second (60Hz)
    timeStep := 1/60;
    velocityIterations := 6;
    positionIterations := 2;

    for i := 0 to 60 do
    begin
      // Instruct the world to perform a single step of simulation.
		  // It is generally best to keep the time step and iterations fixed
      world.Step(timeStep, velocityIterations, positionIterations);

      pos := body.GetPosition;
      angle := body.GetAngle;

      Log(Format('%4.2f   %4.2f   %4.2f', [pos^.x, pos^.y, angle]));
    end;
  finally
    world.Destroy;
  end;
end;

procedure TForm1.Log(const msg: string);
begin
  Memo1.Lines.Add(msg);
end;

end.
