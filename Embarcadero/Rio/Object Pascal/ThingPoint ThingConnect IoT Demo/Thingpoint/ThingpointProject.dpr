program ThingpointProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormU in 'MainFormU.pas' {ThingpointForm},
  NotifyDeviceFrameU in 'NotifyDeviceFrameU.pas' {NotifyDeviceFrame: TFrame},
  CacheDataModuleU in 'CacheDataModuleU.pas' {CacheDataModule: TDataModule},
  EdgeServiceModuleU in 'EdgeServiceModuleU.pas' {EdgeServiceModule: TDataModule},
  ConnectionFrameU in '..\common\ConnectionFrameU.pas' {EMSServerConnectionFrame: TFrame},
  ListenerFrameU in 'ListenerFrameU.pas' {EMSEdgeModuleListenerFrame: TFrame},
  LoggingFrameU in 'LoggingFrameU.pas' {EMSEdgeLoggingFrame: TFrame},
  EdgeModuleResourceU in 'EdgeModuleResourceU.pas',
  StreamingDeviceFrameU in 'StreamingDeviceFrameU.pas' {StreamingDeviceFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCacheDataModule, CacheDataModule);
  Application.CreateForm(TEdgeServiceModule, EdgeServiceModule);
  Application.CreateForm(TThingpointForm, ThingpointForm);
  Application.Run;
end.
