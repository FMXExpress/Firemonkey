program iOSNSOperationQueue;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FNSOperationQueue};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFNSOperationQueue, FNSOperationQueue);
  Application.Run;
end.
