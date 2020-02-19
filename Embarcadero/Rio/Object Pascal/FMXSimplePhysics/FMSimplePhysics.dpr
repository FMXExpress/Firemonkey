
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program FMSimplePhysics;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormFmxPhysics in 'uFormFmxPhysics.pas' {FormFmxPhysics},
  uFmxPhysicsDemo in 'uFmxPhysicsDemo.pas',
  uCustomSimulation in 'uCustomSimulation.pas',
  uFlatBox2DSimulation in 'uFlatBox2DSimulation.pas',
  uSimulationFmxCtrls in 'uSimulationFmxCtrls.pas',
  uFmxControlHelper in 'uFmxControlHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFmxPhysics, FormFmxPhysics);
  Application.Run;
end.
