program TableViewCustomViewSection;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewCustomViewSection};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewCustomViewSection, FTableViewCustomViewSection);
  Application.Run;
end.
