program SettingResettingBadgeNumber;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {SettingBadgeNumberForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSettingBadgeNumberForm, SettingBadgeNumberForm);
  Application.Run;
end.
