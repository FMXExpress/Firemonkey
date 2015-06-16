program TableViewAutoHeight;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableView, FTableView);
  Application.Run;
end.
