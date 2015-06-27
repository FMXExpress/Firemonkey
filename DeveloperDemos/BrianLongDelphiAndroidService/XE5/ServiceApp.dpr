program ServiceApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormU in 'MainFormU.pas' {ServiceInteractionForm},
  ActivityReceiverU in 'ActivityReceiverU.pas',
  ServiceReceiverU in 'ServiceReceiverU.pas',
  ServiceU in 'ServiceU.pas',
  Androidapi.JNI.Toast in 'Androidapi.JNI.Toast.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceInteractionForm, ServiceInteractionForm);
  Application.Run;
end.
