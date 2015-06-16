program iOSiAd;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FiAdBanner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFiAdBanner, FiAdBanner);
  Application.Run;
end.
