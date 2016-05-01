program ShareSheet;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {ShareSheetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TShareSheetForm, ShareSheetForm);
  Application.Run;
end.
