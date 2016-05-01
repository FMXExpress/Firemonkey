
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program FlatBox2DObjPasHelloWorld;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormBox2DHelloWorld in 'uFormBox2DHelloWorld.pas' {Form1},
  Box2D.Dynamics in 'intermediate\Box2D.Dynamics.pas',
  Box2D.Collision in 'intermediate\Box2D.Collision.pas',
  Box2D.Common in 'intermediate\Box2D.Common.pas',
  Box2DTypes in 'intermediate\Box2DTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
