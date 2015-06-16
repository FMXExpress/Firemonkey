program TableViewIndexed;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewIndexed};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewIndexed, FTableViewIndexed);
  Application.Run;
end.
