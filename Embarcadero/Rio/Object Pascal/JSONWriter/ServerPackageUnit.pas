//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ServerPackageUnit;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes,
  REST.Backend.EndPoint;

type
  [ResourceName('JSONWriterReader')]
  TJSONWriterReader = class(TDataModule)
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    procedure Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

uses
  System.JSON.Writers, System.JSON.Builders,
  System.JSON.Readers,
  System.JSON.Types;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TJSONWriterReader.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LEMSEndpointManager: TEMSEndpointManager;
  LEMSResource: TEMSResource;
  LEndPointName: string;
  LBuilder: TJSONObjectBuilder;
begin
  // Write Response using JSONObjectBuilder
  LBuilder := TJSONObjectBuilder.Create(AResponse.Body.JSONWriter);
  try
    LBuilder.BeginObject;
    LEMSEndpointManager := TEMSEndpointManager.Instance;
    for LEMSResource in LEMSEndpointManager.Resources do
    begin
      LBuilder.ParentObject.BeginArray(LEMSResource.Name);
      for LEndPointName in LEMSResource.EndpointNames do
        LBuilder.ParentArray.Add(LEndPointName);
      LBuilder.ParentArray.EndArray;
    end;
  finally
    LBuilder.Free;
  end;
end;

procedure TJSONWriterReader.Post(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LBytes, LBytesResponse: TBytes;
  LReader: TJsonTextReader;
  User, Password, LPropertyName: string;
begin
  User := '';
  Password := '';
  //Read Request Body using JSONReader
  LReader := ARequest.Body.JSONReader;
  LReader.Read;// StartObject
  while LReader.Read and (LReader.TokenType = TJsonToken.PropertyName) do
  begin
    LPropertyName := LReader.Value.AsString;
    LReader.Read;
    if LReader.TokenType <> TJsonToken.String then
      raise Exception.Create('Unexpected Token, expected string');
    if LPropertyName = 'UserName' then
      User := LReader.Value.AsString
    else if LPropertyName = 'Password' then
      Password := LReader.Value.AsString
  end;

  //Write Response using JSONObjectBuilder
  with TJSONObjectBuilder.Create(AResponse.Body.JSONWriter) do
  try
    BeginObject
      .BeginArray('UserDetails')
        .Add(User)
        .Add(Password);
  finally
    Free; // TJSONObjectBuilder
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TJSONWriterReader));
end;

initialization
  Register;
end.


