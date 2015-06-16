//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program Forms;

uses
  System.StartUpCopy,
  FMX.Forms,
  PortraitForm in 'PortraitForm.pas' {PForm},
  LandscapeForm in 'LandscapeForm.pas' {LSForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TPForm, PForm);
  Application.CreateForm(TLSForm, LSForm);
  Application.Run;
end.
