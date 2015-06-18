program EmailCheckerApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormClient in 'uFormClient.pas' {FormClient},
  uDMEmailChecker in 'uDMEmailChecker.pas' {DMEmailChecker: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormClient, FormClient);
  Application.CreateForm(TDMEmailChecker, DMEmailChecker);
  Application.Run;
end.
