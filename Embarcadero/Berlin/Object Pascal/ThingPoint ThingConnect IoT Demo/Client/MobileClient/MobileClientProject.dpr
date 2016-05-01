program MobileClientProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  ConnectionFrameU in '..\..\common\ConnectionFrameU.pas' {EMSServerConnectionFrame: TFrame},
  ClientDataModuleU in '..\ClientDataModuleU.pas' {EMSClientDataModule: TDataModule},
  MainFormU in 'MainFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEMSClientDataModule, EMSClientDataModule);
  //Application.CreateForm(TEMSThingPointForm, EMSThingPointForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
