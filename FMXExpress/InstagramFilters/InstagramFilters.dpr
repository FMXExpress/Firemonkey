program InstagramFilters;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uMain in 'uMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
