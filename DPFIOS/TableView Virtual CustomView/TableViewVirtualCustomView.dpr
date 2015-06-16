program TableViewVirtualCustomView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewVirtualCustomView};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewVirtualCustomView, FTableViewVirtualCustomView);
  Application.Run;
end.
