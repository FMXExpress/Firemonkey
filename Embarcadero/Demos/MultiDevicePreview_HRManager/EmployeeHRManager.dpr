program EmployeeHRManager;

uses
  System.StartUpCopy,
  FMX.Forms,
  HRManager in 'HRManager.pas' {HRManagerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THRManagerForm, HRManagerForm);
  Application.Run;
end.
