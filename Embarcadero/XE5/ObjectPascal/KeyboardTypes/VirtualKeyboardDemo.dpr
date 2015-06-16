//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program VirtualKeyboardDemo;

uses
  FMX.Types,
  FMX.Forms,
  VirtualKeyboardBase in 'VirtualKeyboardBase.pas' {VKBaseForm};

{$R *.res}

begin
  Application.Initialize;
  VKAutoShowMode := TVKAutoShowMode.vkasAlways;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait, TFormOrientation.soInvertedPortrait, TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TVKBaseForm, VKBaseForm);
  Application.Run;
end.
