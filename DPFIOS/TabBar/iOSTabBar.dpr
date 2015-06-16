Program iOSTabBar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTabBar};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFTabBar, FTabBar);
  Application.Run;

End.
