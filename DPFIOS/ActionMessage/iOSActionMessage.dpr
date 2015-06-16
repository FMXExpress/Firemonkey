Program iOSActionMessage;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FActionMessages};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFActionMessages, FActionMessages);
  Application.Run;

End.
