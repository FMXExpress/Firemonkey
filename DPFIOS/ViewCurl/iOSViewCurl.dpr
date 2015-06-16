Program iOSViewCurl;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FViewCurl};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFViewCurl, FViewCurl);
  Application.Run;

End.
