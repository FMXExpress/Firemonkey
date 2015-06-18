program HeartRateMonitor1;



uses
  System.StartUpCopy,
  FMX.Forms,
  UHeartRateForm in 'UHeartRateForm.pas' {frmHeartMonitor},
  Login in 'Login.pas' {loginForm};

begin
  Application.Initialize;
  Application.CreateForm(TloginForm, loginForm);
  Application.CreateForm(TfrmHeartMonitor, frmHeartMonitor);
  Application.Run;
end.
