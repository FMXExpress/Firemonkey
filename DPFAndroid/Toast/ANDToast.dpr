program ANDToast;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FToast};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFToast, FToast);
  Application.Run;
end.
