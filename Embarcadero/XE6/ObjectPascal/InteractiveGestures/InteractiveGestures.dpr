
//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program InteractiveGestures;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form36};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm36, Form36);
  Application.Run;
end.
