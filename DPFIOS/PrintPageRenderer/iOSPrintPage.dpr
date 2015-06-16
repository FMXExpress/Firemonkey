program iOSPrintPage;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPrintPage};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPrintPage, FPrintPage);
  Application.Run;
end.
