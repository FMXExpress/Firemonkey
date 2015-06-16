program iOSPageView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FNavigationControllerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFNavigationControllerForm, FNavigationControllerForm);
  Application.Run;
end.
