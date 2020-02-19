program ClientProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormU in 'MainFormU.pas' {EMSThingPointForm},
  ClientDataModuleU in '..\ClientDataModuleU.pas' {EMSClientDataModule: TDataModule},
  EndpointResultFrameU in '..\EndpointResultFrameU.pas' {EMSEndpointResultFrame: TFrame},
  ClientSettingsU in '..\ClientSettingsU.pas',
  ConnectionFrameU in '..\..\common\ConnectionFrameU.pas' {EMSServerConnectionFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEMSClientDataModule, EMSClientDataModule);
  Application.CreateForm(TEMSThingPointForm, EMSThingPointForm);
  Application.Run;
end.
