//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program HeartRateMonitor;

uses
  System.StartUpCopy,
  FMX.Forms,
  UHeartRateForm in 'UHeartRateForm.pas' {frmHeartMonitor};

begin
  Application.Initialize;
  Application.CreateForm(TfrmHeartMonitor, frmHeartMonitor);
  Application.Run;
end.
