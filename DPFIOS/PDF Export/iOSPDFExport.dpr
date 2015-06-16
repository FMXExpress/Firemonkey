program iOSPDFExport;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPDFExport};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPDFExport, FPDFExport);
  Application.Run;
end.
