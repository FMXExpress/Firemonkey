Program iOSCheckBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FCheckBox};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFCheckBox, FCheckBox);
  Application.Run;

End.
