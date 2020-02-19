//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program TestBed;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {TestBedForm},
  Test in 'Test.pas',
  Chain in 'Tests\Chain.pas',
  VaryingRestitution in 'Tests\VaryingRestitution.pas',
  Tiles in 'Tests\Tiles.pas',
  AddPair in 'Tests\AddPair.pas',
  ApplyForce in 'Tests\ApplyForce.pas',
  BasicSliderCrank in 'Tests\BasicSliderCrank.pas',
  BodyTypes in 'Tests\BodyTypes.pas',
  Breakable in 'Tests\Breakable.pas',
  Bridge in 'Tests\Bridge.pas',
  BulletTest in 'Tests\BulletTest.pas',
  Cantilever in 'Tests\Cantilever.pas',
  Car in 'Tests\Car.pas',
  CharacterCollision in 'Tests\CharacterCollision.pas',
  CollisionFiltering in 'Tests\CollisionFiltering.pas',
  CollisionProcessing in 'Tests\CollisionProcessing.pas',
  CompoundShapes in 'Tests\CompoundShapes.pas',
  Confined in 'Tests\Confined.pas',
  ContinuousTest in 'Tests\ContinuousTest.pas',
  ConvexHull in 'Tests\ConvexHull.pas',
  ConveyorBelt in 'Tests\ConveyorBelt.pas',
  DistanceTest in 'Tests\DistanceTest.pas',
  Dominos in 'Tests\Dominos.pas',
  DumpShell in 'Tests\DumpShell.pas',
  EdgeShapes in 'Tests\EdgeShapes.pas',
  EdgeTest in 'Tests\EdgeTest.pas',
  Gears in 'Tests\Gears.pas',
  HeavyOnLight in 'Tests\HeavyOnLight.pas',
  HeavyOnLightTwo in 'Tests\HeavyOnLightTwo.pas',
  Mobile in 'Tests\Mobile.pas',
  MobileBalanced in 'Tests\MobileBalanced.pas',
  MotorJoint in 'Tests\MotorJoint.pas',
  OneSidedPlatform in 'Tests\OneSidedPlatform.pas',
  Pinball in 'Tests\Pinball.pas',
  PolyCollision in 'Tests\PolyCollision.pas',
  Prismatic in 'Tests\Prismatic.pas',
  Pulleys in 'Tests\Pulleys.pas',
  Pyramid in 'Tests\Pyramid.pas',
  Revolute in 'Tests\Revolute.pas',
  Rope in 'Tests\Rope.pas',
  RopeJoint in 'Tests\RopeJoint.pas',
  SensorTest in 'Tests\SensorTest.pas',
  ShapeEditing in 'Tests\ShapeEditing.pas',
  SliderCrank in 'Tests\SliderCrank.pas',
  SphereStack in 'Tests\SphereStack.pas',
  TheoJansen in 'Tests\TheoJansen.pas',
  TimeOfImpact in 'Tests\TimeOfImpact.pas',
  Tumbler in 'Tests\Tumbler.pas',
  VaryingFriction in 'Tests\VaryingFriction.pas',
  VerticalStack in 'Tests\VerticalStack.pas',
  Web in 'Tests\Web.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestBedForm, TestBedForm);
  Application.Run;
end.
