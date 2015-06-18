program GettingStarted;





uses
  FMX.Forms,
  UGettingStarted in 'UGettingStarted.pas' {FGettingStarted},
  UPaths in '..\..\..\Shared\UPaths.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFGettingStarted, FGettingStarted);
  Application.Run;
end.
