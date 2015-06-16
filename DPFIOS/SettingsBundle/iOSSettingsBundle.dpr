program iOSSettingsBundle;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FSettingsBundle};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFSettingsBundle, FSettingsBundle);
  Application.Run;
end.
