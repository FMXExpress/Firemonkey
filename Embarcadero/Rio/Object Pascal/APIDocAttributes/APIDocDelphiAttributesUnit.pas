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
unit APIDocDelphiAttributesUnit;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.VCLUI.Wait, FireDAC.Stan.StorageJSON, FireDAC.Phys.IBBase,
  FireDAC.Comp.UI, APIDocDelphiAttributesUnitConsts, REST.Response.Adapter, FireDAC.Stan.StorageBin;

type
  [ResourceName('SampleAttributesDelphi')]
  [EndPointObjectsYAMLDefinitions(cYamlDefinitions)]
  [EndPointObjectsJSONDefinitions(cJSONDefinitions)]

  TSampleAttributesDelphiResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    EmployeeTable: TFDQuery;
    FDSchemaAdapterEmployees: TFDSchemaAdapter;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    procedure DataModuleCreate(Sender: TObject);
  published
    [EndPointRequestSummary('Sample Tag', 'Summary Title', 'Get Method Description', 'application/json', '')]
    [EndPointRequestParameter(TAPIDocParameter.TParameterIn.Query, 'skip', 'Query Parameter Description',
      false, TAPIDoc.TPrimitiveType.spNumber, TAPIDoc.TPrimitiveFormat.Int32, TAPIDoc.TPrimitiveType.spNull, '', '')]
    [EndPointResponseDetails(200, 'Ok', TAPIDoc.TPrimitiveType.spObject, TAPIDoc.TPrimitiveFormat.None, '', '#/definitions/EmployeeTable')]
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('{item}')]
    [EndPointRequestSummary('Sample Tag', 'Summary Title', 'GetItem Method Description', 'application/json', '')]
    [EndPointRequestParameter(TAPIDocParameter.TParameterIn.Path, 'item', 'Path Parameter item Description', true, TAPIDoc.TPrimitiveType.spString,
      TAPIDoc.TPrimitiveFormat.None, TAPIDoc.TPrimitiveType.spString, '', '')]
    [EndPointResponseDetails(200, 'Ok', TAPIDoc.TPrimitiveType.spObject, TAPIDoc.TPrimitiveFormat.None, '', '#/definitions/EmployeeTable')]
    [EndPointResponseDetails(404, 'Not Found', TAPIDoc.TPrimitiveType.spNull, TAPIDoc.TPrimitiveFormat.None, '', '')]
    procedure GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [EndPointRequestSummary('Sample Tag', 'Summary Title', 'Post Method Description', 'application/json', 'application/json')]
    [EndPointRequestParameter(TAPIDocParameter.TParameterIn.Body, 'body', 'Body Param Description', true, TAPIDoc.TPrimitiveType.spObject,
      TAPIDoc.TPrimitiveFormat.None, TAPIDoc.TPrimitiveType.spObject, '', '#/definitions/PostObject')]
    [EndPointResponseDetails(200, 'Ok', TAPIDoc.TPrimitiveType.spNull, TAPIDoc.TPrimitiveFormat.None, '', '#/definitions/ItemPostedResponseObject')]
    [EndPointResponseDetails(409, ' Item Exist', TAPIDoc.TPrimitiveType.spNull, TAPIDoc.TPrimitiveFormat.None, '', '')]
    procedure Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('{item}')]
    [EndPointRequestSummary('Sample Tag', 'Summary Title', 'PutItem Method Description', 'application/json', 'application/json')]
    [EndPointRequestParameter(TAPIDocParameter.TParameterIn.Path, 'item', 'Path Parameter item Description', true, TAPIDoc.TPrimitiveType.spString,
      TAPIDoc.TPrimitiveFormat.None, TAPIDoc.TPrimitiveType.spString, '', '')]
    [EndPointRequestParameter(TAPIDocParameter.TParameterIn.Body, 'body', 'Body Param Description', true, TAPIDoc.TPrimitiveType.spObject,
      TAPIDoc.TPrimitiveFormat.None, TAPIDoc.TPrimitiveType.spObject, '', '#/definitions/PutObject')]
    [EndPointResponseDetails(200, 'Ok', TAPIDoc.TPrimitiveType.spObject, TAPIDoc.TPrimitiveFormat.None, '', '#/definitions/ItemPutResponseObject')]
    [EndPointResponseDetails(404, 'Not Found', TAPIDoc.TPrimitiveType.spNull, TAPIDoc.TPrimitiveFormat.None, '', '')]
    procedure PutItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  Data.FireDACJSONReflect, System.JSON.Readers, System.JSON.Types, System.JSON.Builders, System.DateUtils;

{$R *.dfm}

procedure TSampleAttributesDelphiResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  MemStream: TMemoryStream;
begin
  EmployeeTable.SQL.Clear;
  EmployeeTable.SQL.Add('SELECT *  FROM EMPLOYEE');
  EmployeeTable.Open;
  MemStream := TMemoryStream.Create;
  try
    FDSchemaAdapterEmployees.SaveToStream(MemStream, TFDStorageFormat.sfJSON);
    AResponse.Body.SetStream(MemStream, 'application/json', True);
  except
    MemStream.Free;
  end;
end;

procedure TSampleAttributesDelphiResource1.GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
  MemStream: TMemoryStream;
  LJSONObjectBuilder: TJSONObjectBuilder;
begin
  LItem := ARequest.Params.Values['item'];
  EmployeeTable.SQL.Clear;
  EmployeeTable.SQL.Add('SELECT *  FROM EMPLOYEE WHERE FIRST_NAME = ''' + LItem + '''');
  EmployeeTable.Open;
  if  EmployeeTable.RecordCount <= 0 then
  begin
    LJSONObjectBuilder := TJSONObjectBuilder.Create(AResponse.Body.JSONWriter);
    try
      AResponse.StatusCode := 404;
      LJSONObjectBuilder.BeginObject.Add(sErrorMessage, sError)
        .Add(sDescription, sNotFound);
    finally
      LJSONObjectBuilder.Free;
    end;
  end
  else
  begin
    MemStream := TMemoryStream.Create;
    try
      FDSchemaAdapterEmployees.SaveToStream(MemStream, TFDStorageFormat.sfJSON);
      AResponse.Body.SetStream(MemStream, 'application/json', True);
    except
      MemStream.Free;
    end;
  end;
end;

procedure TSampleAttributesDelphiResource1.Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LBytes, LBytesResponse: TBytes;
  LReader: TJsonTextReader;
  LPropertyName: string;
  LJSONObjectBuilder: TJSONObjectBuilder;
begin
  //Read Request Body using JSONReader
  LReader := ARequest.Body.JSONReader;
  LReader.Read;

  LJSONObjectBuilder := TJSONObjectBuilder.Create(AResponse.Body.JSONWriter);
  try
    LJSONObjectBuilder.BeginObject.BeginArray(sPostedData);
    while LReader.Read and (LReader.TokenType = TJsonToken.PropertyName) do
    begin
      LPropertyName := LReader.Value.AsString;
      LReader.Read;
      if (LReader.TokenType <> TJsonToken.String) and (LReader.TokenType <> TJsonToken.Integer) then
        raise Exception.Create(sUnExpectedToken);
      if LPropertyName = 'EMP_NO' then
      begin
       LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
      else if LPropertyName = 'FIRST_NAME' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'LAST_NAME' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'PHONE_EXT' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'HIRE_DATE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'DEPT_NO' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'JOB_CODE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'JOB_GRADE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
      else if LPropertyName = 'JOB_COUNTRY' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'SALARY' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
    end;
  finally
    LJSONObjectBuilder.Free;
  end;
end;

procedure TSampleAttributesDelphiResource1.PutItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
var
  LBytes, LBytesResponse: TBytes;
  LReader: TJsonTextReader;
  LPropertyName: string;
  LJSONObjectBuilder: TJSONObjectBuilder;
begin
  LItem := ARequest.Params.Values['item'];
  //Read Request Body using JSONReader
  LReader := ARequest.Body.JSONReader;
  LReader.Read;

  LJSONObjectBuilder := TJSONObjectBuilder.Create(AResponse.Body.JSONWriter);
  try
    LJSONObjectBuilder.BeginObject.Add(sPathItem, LItem )
      .BeginArray(sPostedData);
    while LReader.Read and (LReader.TokenType = TJsonToken.PropertyName) do
    begin
      LPropertyName := LReader.Value.AsString;
      LReader.Read;
      if (LReader.TokenType <> TJsonToken.String) and (LReader.TokenType <> TJsonToken.Integer) then
        raise Exception.Create(sUnExpectedToken);
      if LPropertyName = 'EMP_NO' then
      begin
       LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
      else if LPropertyName = 'FIRST_NAME' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'LAST_NAME' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'PHONE_EXT' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'HIRE_DATE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'DEPT_NO' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'JOB_CODE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'JOB_GRADE' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
      else if LPropertyName = 'JOB_COUNTRY' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsString);
      end
      else if LPropertyName = 'SALARY' then
      begin
         LJSONObjectBuilder.ParentArray.Add(LReader.Value.AsInteger.ToString);
      end
    end;
  finally
    LJSONObjectBuilder.Free;
  end;
end;

procedure TSampleAttributesDelphiResource1.DataModuleCreate(Sender: TObject);
begin
  EmployeeConnection.Connected := True;
  EmployeeTable.Active := True;
end;


procedure Register;
begin
  RegisterResource(TypeInfo(TSampleAttributesDelphiResource1));
end;

initialization
  Register;
end.


