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
