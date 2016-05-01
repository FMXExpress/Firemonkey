program TabControlDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitData in 'UnitData.pas' {MainDataModule: TDataModule},
  UnitMain in 'UnitMain.pas' {HeaderFooterwithNavigation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainDataModule, MainDataModule);
  Application.CreateForm(THeaderFooterwithNavigation, HeaderFooterwithNavigation);
  Application.Run;
end.
