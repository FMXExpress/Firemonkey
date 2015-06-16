program iOSComboBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FComboBox};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFComboBox, FComboBox);
  Application.Run;

end.
