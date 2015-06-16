program iOSQPDFViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FQuickLook};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm( TFQuickLook, FQuickLook );
  Application.Run;

end.
