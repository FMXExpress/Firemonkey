
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
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
  DebugDraw in 'DebugDraw.pas',
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
  Cantilever in 'Tests\Cantilever.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestBedForm, TestBedForm);
  Application.Run;
end.
