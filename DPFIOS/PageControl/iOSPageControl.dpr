program iOSPageControl;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPageControl};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPageControl, FPageControl);
  Application.Run;
end.
