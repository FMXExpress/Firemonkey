program Plugins;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {fMain},
  FMX.Features.BitmapHelper in 'FMX.Features.BitmapHelper.pas',
  uPlugin in 'uPlugin.pas',
  uUtils in 'uUtils.pas',
  uPluginEdit in 'uPluginEdit.pas' {fPluginEdit},
  OpenViewUrl in 'OpenViewUrl.pas',
  uShow in 'uShow.pas' {fShow},
  ufrPlugins in 'ufrPlugins.pas' {frPlugins: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfPluginEdit, fPluginEdit);
  Application.CreateForm(TfShow, fShow);
  Application.Run;
end.
