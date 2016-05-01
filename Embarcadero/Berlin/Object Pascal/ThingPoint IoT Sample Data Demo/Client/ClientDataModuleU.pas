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
unit ClientDataModuleU;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  System.JSON, REST.Backend.EMSServices, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint,
  REST.Backend.EMSProvider, REST.Backend.MetaTypes, REST.Backend.BindSource,
  REST.Backend.ServiceComponents;

type
  TEMSClientDataModule = class(TDataModule)
    EMSProvider1: TEMSProvider;
    BackendEndpointMeasurements: TBackendEndpoint;
    BackendEndpointEdgeMeasurements: TBackendEndpoint;
    BackendQueryEdgeModules: TBackendQuery;
    BackenEndpointEdgeDetailedMeasurements: TBackendEndpoint;
  private
    function GetCurrentEdge: string;
    procedure SetCurrentEdge(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    function GetEdgeNames: TArray<string>;
    property CurrentEdge: string read GetCurrentEdge write SetCurrentEdge;
  end;

var
  EMSClientDataModule: TEMSClientDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TEMSClientDataModule }

function TEMSClientDataModule.GetCurrentEdge: string;
begin
  Result := BackendEndpointEdgeMeasurements.Params[0].Value;
end;

function TEMSClientDataModule.GetEdgeNames: TArray<string>;
var
  LJSONValue: TJSONValue;
begin
  BackendQueryEdgeModules.Execute;
  if BackendQueryEdgeModules.JSONResult <> nil then
    for LJSONValue in BackendQueryEdgeModules.JSONResult do
      Result := Result + [LJSONValue.GetValue<string>('modulename')];
end;

procedure TEMSClientDataModule.SetCurrentEdge(const Value: string);
begin
  BackendEndpointEdgeMeasurements.Params[0].Value := Value;
  BackenEndpointEdgeDetailedMeasurements.Params[0].Value := Value;
end;

end.
