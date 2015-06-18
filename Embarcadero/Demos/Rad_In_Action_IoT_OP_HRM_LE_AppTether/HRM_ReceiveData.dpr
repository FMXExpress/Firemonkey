program HRM_ReceiveData;

uses
  System.StartUpCopy,
  FMX.Forms,
  uReceiveData in 'uReceiveData.pas' {frmHRMDataReceiver};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmHRMDataReceiver, frmHRMDataReceiver);
  Application.Run;
end.
