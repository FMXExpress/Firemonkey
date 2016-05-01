program BeaconService;

uses
  System.Android.ServiceApplication,
  BeaconServiceUnit in 'BeaconServiceUnit.pas' {BeaconServiceDM: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBeaconServiceDM, BeaconServiceDM);
  Application.Run;
end.
