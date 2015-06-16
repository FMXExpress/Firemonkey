program TableView2;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableView};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm( TFTableView, FTableView );
  Application.Run;

end.
