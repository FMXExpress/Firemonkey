//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program SettingsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  SettingsProjectForm in 'SettingsProjectForm.pas' {SettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
end.
