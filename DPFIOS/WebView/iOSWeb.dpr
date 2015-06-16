Program iOSWeb;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FWebView};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFWebView, FWebView);
  Application.Run;

End.
