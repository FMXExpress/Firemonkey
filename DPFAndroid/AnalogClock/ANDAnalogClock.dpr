program ANDAnalogClock;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FAnalogClock};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFAnalogClock, FAnalogClock);
  Application.Run;
end.
