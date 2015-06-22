program landscapeMobile;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  landscapeMainForm in 'landscapeMainForm.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
