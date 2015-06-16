Program iOSDatePicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDatePicker};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFDatePicker, FDatePicker);
  Application.Run;

End.
