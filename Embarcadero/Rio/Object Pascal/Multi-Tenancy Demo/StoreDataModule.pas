// Copyright (c) 2017 Embarcadero Technologies, Inc. All rights reserved.  
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
unit StoreDataModule;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Response.Adapter,
  FireDAC.UI.Intf, FireDAC.ConsoleUI.Wait, FireDAC.Stan.StorageJSON,
  FireDAC.Comp.UI, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.DApt,
  CommonDataModule, FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase;

type
  [ResourceName('items')]
  TStoreResource = class(TDataModule)
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    QueryGetItems: TFDQuery;
    FDSchemaAdapter: TFDSchemaAdapter;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
  private
    procedure GetData(const ATenantId: string);
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    procedure Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
 end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  REST.Types;

procedure TStoreResource.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    GetData(AContext.Tenant.Id);
    FDSchemaAdapter.SaveToStream(LStream, TFDStorageFormat.sfJSON);
    AResponse.Body.SetStream(LStream, CONTENTTYPE_APPLICATION_VND_EMBARCADERO_FIREDAC_JSON, True);
  except
    LStream.Free;
    raise;
  end;
end;

procedure TStoreResource.GetData(const ATenantId: string);
begin
  CommonDM.CheckConnected;
  QueryGetItems.Params.ParamByName('TenantId').AsString := ATenantId;
  QueryGetItems.Open;
end;

procedure TStoreResource.Post(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LStream: TStream;
begin
  if not SameText(ARequest.Body.ContentType, CONTENTTYPE_APPLICATION_VND_EMBARCADERO_FIREDAC_JSON) then
    AResponse.RaiseBadRequest('content type');

  if not ARequest.Body.TryGetStream(LStream) then
    AResponse.RaiseBadRequest('no stream');

  LStream.Position := 0;
  FDSchemaAdapter.LoadFromStream(LStream, TFDStorageFormat.sfJSON);
  FDSchemaAdapter.ApplyUpdates;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TStoreResource));
end;

initialization
  Register;
end.


