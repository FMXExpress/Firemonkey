program TableViewSearch;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewSearch};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm( TFTableViewSearch, FTableViewSearch );
  Application.Run;

end.
