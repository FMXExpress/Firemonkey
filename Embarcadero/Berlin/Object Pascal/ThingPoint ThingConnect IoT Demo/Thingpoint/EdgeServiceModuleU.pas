//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit EdgeServiceModuleU;

interface

uses
  System.SysUtils, System.Classes, EMSHosting.ExtensionsServices,
  EMSHosting.EdgeHTTPListener, REST.Backend.EMSServices, System.JSON,
  IPPeerClient, REST.Backend.EMSProvider, REST.Backend.Providers,
  EMSHosting.EdgeService;

type
  TEdgeServiceModule = class(TDataModule)
    EMSEdgeService1: TEMSEdgeService;
    EMSProvider1: TEMSProvider;
    procedure EMSEdgeService1Registering(Sender: TObject;
      const AModuleDetail: TJSONObject; const AResources: TJSONArray;
      var AHandled: Boolean);
    procedure EMSEdgeService1Registered(Sender: TObject);
  public type
    TOnModuleOvewriteCallback = reference to function(const AModuleName: string): Boolean;
    TOnProtocolPropsConflictCallback = reference to function(const AModuleName, AProtocolProps: string): Boolean;
  private
    FOnModuleOverwrite: TOnModuleOvewriteCallback;
    FOnProtocolPropsConflictDelete: TOnProtocolPropsConflictCallback;
    { Private declarations }
  public
    { Public declarations }
    property OnModuleOverwrite: TOnModuleOvewriteCallback read FOnModuleOverwrite write FOnModuleOverwrite;
    property OnProtocolPropsConflictDelete: TOnProtocolPropsConflictCallback read FOnProtocolPropsConflictDelete write FOnProtocolPropsConflictDelete;
  end;

var
  EdgeServiceModule: TEdgeServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses REST.Backend.EMSAPI;

procedure TEdgeServiceModule.EMSEdgeService1Registered(Sender: TObject);
var
  LAPI: TEMSClientAPI;
  LEdgeService: TEMSEdgeService;
  LDelete: Boolean;
  LModules: TArray<TEMSClientApi.TModule>;
  LModule: TEMSClientApi.TModule;
  LProtocolProps: string;
begin
  // Check for modules with the same connection properties

  LEdgeService :=  Sender as TEMSEdgeService;
  LAPI := (LEdgeService.ProviderService as IGetEMSApi).EMSAPI;
  // Get this module
  LAPI.QueryModuleName(LEdgeService.ModuleName,
     procedure(const AModule: TEMSClientAPI.TModule; const AObj: TJSONObject)
     begin
       LProtocolProps := AModule.ProtocolProps;
     end);

  // Get all other modules
  LAPI.QueryModules([Format('where={"modulename":{"$ne":"%s"}}', [LEdgeService.ModuleName])], nil, LModules);
  for LModule in LModules do
  begin
    if SameText(LModule.ProtocolProps, LProtocolProps) then
    begin
    if Assigned(FOnProtocolPropsConflictDelete) then
      LDelete := FOnProtocolPropsConflictDelete(LModule.ModuleName, LProtocolProps)
    else
      LDelete := False;
    if LDelete then
      LAPI.UnregisterModule(LModule.ModuleID);

    end;
  end;
end;

procedure TEdgeServiceModule.EMSEdgeService1Registering(Sender: TObject;
  const AModuleDetail: TJSONObject; const AResources: TJSONArray;
  var AHandled: Boolean);
var
  LAPI: TEMSClientAPI;
  LEdgeService: TEMSEdgeService;
  LModuleID: string;
  LDelete: Boolean;
  LCancel: Boolean;
begin

  // Check for module with the same name

  LCancel := False;
  LEdgeService :=  Sender as TEMSEdgeService;
  if LEdgeService.ModuleID = '' then
  begin
    LAPI := (LEdgeService.ProviderService as IGetEMSApi).EMSAPI;
    LAPI.QueryModuleName(LEdgeService.ModuleName,
       procedure(const AModule: TEMSClientAPI.TModule; const AObj: TJSONObject)
       begin
         LModuleID := AModule.ModuleID;
       end);
    if LModuleID <> '' then
    begin
      if Assigned(FOnModuleOverwrite) then
        LDelete := FOnModuleOverwrite(LEdgeService.ModuleName)
      else
        LDelete := False;

      if LDelete then
        LAPI.UnregisterModule(LModuleID);
    end;
  end;
  if LCancel then
    AHandled := True;



end;

end.
