
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program CPUMonitor;

uses
  FMX.Forms,
  MonitorForm in 'MonitorForm.pas' {MonitorGuageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMonitorGuageForm, MonitorGuageForm);
  Application.Run;
end.
