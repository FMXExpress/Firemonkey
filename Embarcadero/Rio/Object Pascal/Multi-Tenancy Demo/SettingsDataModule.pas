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
unit SettingsDataModule;

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
  CommonDataModule;

type
  [ResourceName('settings')]
  [AllowAnonymousTenant]
  TSettingsResource = class(TDataModule)
    QueryGetActiveStores: TFDQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  published
    [EndpointName('stores')]
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

var
  SettingsResource: TSettingsResource;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TSettingsDM }

procedure TSettingsResource.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    CommonDM.CheckConnected;
    QueryGetActiveStores.Open;
    QueryGetActiveStores.SaveToStream(LStream, TFDStorageFormat.sfJSON);
    AResponse.Body.SetStream(LStream, 'application\json', True);
  finally
  //  LStream.Free;
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TSettingsResource));
end;

initialization
  Register;
end.
