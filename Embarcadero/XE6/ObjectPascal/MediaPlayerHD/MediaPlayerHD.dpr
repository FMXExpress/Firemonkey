
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program MediaPlayerHD;

uses
  FMX.Forms,
  MediaPlayerForm in 'MediaPlayerForm.pas' {Form240};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm240, Form240);
  Application.Run;
end.
