program TableViewDyn;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewDyn},
  uSecForm in 'uSecForm.pas' {FSecForm};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewDyn, FTableViewDyn);
  Application.CreateForm(TFSecForm, FSecForm);
  Application.Run;

end.
