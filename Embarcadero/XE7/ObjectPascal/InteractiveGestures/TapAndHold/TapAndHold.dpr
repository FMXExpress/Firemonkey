program TapAndHold;

uses
  System.StartUpCopy,
  FMX.Forms,
  TapHoldForm in 'TapHoldForm.pas' {TapHold};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTapHold, TapHold);
  Application.Run;
end.
