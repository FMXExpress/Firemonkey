program ANDChronometer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FChronometer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFChronometer, FChronometer);
  Application.Run;
end.
