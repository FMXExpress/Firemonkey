program MobileWrappers;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  OpenViewUrl in 'OpenViewUrl.pas',
  NetworkState in 'NetworkState.pas',
  SendMail in 'SendMail.pas',
  uSplashScreen in 'uSplashScreen.pas' {SpashScreenForm},
  BarCodeReader in 'BarCodeReader.pas',
  ToastMessage in 'ToastMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
