program TableViewValue12;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewValue12};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewValue12, FTableViewValue12);
  Application.Run;
end.
