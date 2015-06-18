program DelphiVacationCountdown;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
