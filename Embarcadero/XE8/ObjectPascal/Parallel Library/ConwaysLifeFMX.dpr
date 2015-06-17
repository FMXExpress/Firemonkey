program ConwaysLifeFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  LifeUnitFmx in 'LifeUnitFmx.pas' {LifeForm},
  LifeEngine in '..\Shared_Code\LifeEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLifeForm, LifeForm);
  Application.Run;
end.
