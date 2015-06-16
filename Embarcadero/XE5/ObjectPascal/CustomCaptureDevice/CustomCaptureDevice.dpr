
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program CustomCaptureDevice;

uses
  FMX.Forms,
  CustomCaptureDeviceFrm in 'CustomCaptureDeviceFrm.pas' {Form272},
  CustomCaptureDeviceUnit in 'CustomCaptureDeviceUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm272, Form272);
  Application.Run;
end.
