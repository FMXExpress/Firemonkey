
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program FlyoutDemo;

uses
  FMX.Forms,
  Flyouts in 'Flyouts.pas' {FlyoutDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFlyoutDemoForm, FlyoutDemoForm);
  Application.Run;
end.
