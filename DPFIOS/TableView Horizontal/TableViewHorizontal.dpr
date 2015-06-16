program TableViewHorizontal;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableView1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableView1, FTableView1);
  Application.Run;
end.
