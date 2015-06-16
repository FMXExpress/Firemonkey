program iOSRateApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FRateApp};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFRateApp, FRateApp);
  Application.Run;
end.
