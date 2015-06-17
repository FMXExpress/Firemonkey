
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program GlassLevel;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uGlassLevel in 'uGlassLevel.pas' {Form2},
  Androidapi.JNI.PowerManager in 'Androidapi.JNI.PowerManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
