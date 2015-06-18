program Location;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {LocationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLocationForm, LocationForm);
  Application.Run;
end.
