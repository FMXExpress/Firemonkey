program iOSMapViewDirection;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FMapView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMapView, FMapView);
  Application.Run;
end.
