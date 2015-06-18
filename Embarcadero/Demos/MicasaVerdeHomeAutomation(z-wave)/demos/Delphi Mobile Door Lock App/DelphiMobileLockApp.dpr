program DelphiMobileLockApp;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Unit6 in 'Unit6.pas' {Form6};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
