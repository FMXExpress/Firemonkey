program iOSTextField;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTextField};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTextField, FTextField);
  Application.Run;

end.
