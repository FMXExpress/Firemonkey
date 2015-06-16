program ANDTextClock;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTextClock};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTextClock, FTextClock);
  Application.Run;
end.
