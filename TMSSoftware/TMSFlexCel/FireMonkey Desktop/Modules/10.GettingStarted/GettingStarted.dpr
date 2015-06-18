program GettingStarted;





{$R *.dres}

uses
  FMX.Forms,
  UGettingStarted in 'UGettingStarted.pas' {FGettingStarted};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFGettingStarted, FGettingStarted);
  Application.Run;
end.
