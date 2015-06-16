
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program Cubes;

uses
  FMX.Forms,
  FMX.Types,
  CubesFrm in 'CubesFrm.pas' {Form268};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm268, Form268);
  Application.Run;
end.
