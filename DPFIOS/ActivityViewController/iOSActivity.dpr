program iOSActivity;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FLabels};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm( TFActivityViewController, FActivityViewController );
  Application.Run;

end.
