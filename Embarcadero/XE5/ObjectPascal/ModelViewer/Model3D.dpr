
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program Model3D;

uses
  FMX.Forms,
  Model3D_U in 'Model3D_U.pas' {Model3DTest},
  FrameMaterial3DDesigner in 'FrameMaterial3DDesigner.pas' {FrameMaterialDesigner: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TModel3DTest, Model3DTest);
  Application.Run;
end.
