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
unit CustomResourceU;

// EMS Resource Unit

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI,
  EMS.ResourceTypes, System.Generics.Collections;

type
  [ResourceName('Measurements')]
{$METHODINFO ON}

  TMeasurementsResource = class
  private
    function GetModuleNames(const AContext: TEndpointContext): TArray<string>;
  published
    procedure Get(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;
{$METHODINFO OFF}

implementation

// Aggregate data from all "Measurements" resources
procedure TMeasurementsResource.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
type
  TStringPair = TPair<string, string>;
var
  LAPI: TEMSInternalAPI;
  LResponse: IEMSResourceResponseContent;
  LModuleName: string;
  LResultArray: TJSONArray;
  LResponseArray: TJSONArray;
  LResultObject: TJSONObject;
  LModuleNames: TArray<string>;
begin
  LModuleNames := GetModuleNames(AContext);
  LResultArray := nil;
  LResultObject := nil;
  LAPI := TEMSInternalAPI.Create(AContext);
  try
    LResultArray := TJSONArray.Create;
    for LModuleName in LModuleNames do
    begin
      try
        LResponse := LAPI.ModuleResourceGet(LModuleName, 'Measurements',
          nil, nil);
        if LResponse.TryGetArray(LResponseArray) then
        begin
          LResultObject := TJSONObject.Create;
          LResultObject.AddPair('edge', LModuleName);
          LResultObject.AddPair('data', LResponseArray.Clone as TJSONArray);
          LResultArray.AddElement(LResultObject);
          LResultObject := nil;
        end;
      except
        // Error contacting edgemodule
        on E: Exception do
        begin
          LResultObject := TJSONObject.Create;
          LResultObject.AddPair('edge', LModuleName);
          LResultObject.AddPair('error', E.Message);
          LResultArray.AddElement(LResultObject);
          LResultObject := nil;
       end;
      end;
     end;

    AResponse.Body.SetValue(LResultArray, True);
    LResultArray := nil;

  finally
    LAPI.Free;
    LResultArray.Free;
    LResultObject.Free;
  end;
end;

// Get the names of the edgemodules with a resource called "Measurements"
function TMeasurementsResource.GetModuleNames(const AContext: TEndpointContext): TArray<string>;
type
  TStringPair = TPair<string, string>;
var
  LAPI: TEMSInternalAPI;
  LResponse: IEMSResourceResponseContent;
  LJSONArray: TJSONArray;
  LJSONValue: TJSONValue;
  LResourceName: string;
  LModuleName: string;
  LQuery: TEMSInternalAPI.TQueryParams;
  LModuleNames: TArray<string>;
begin
  LAPI := TEMSInternalAPI.Create(AContext);
  try
    LQuery := TEMSInternalAPI.TQueryParams.Create
      (TEMSInternalAPI.TQueryParam.Create('where',
      '{"resourcename":"Measurements"}'));
    LResponse := LAPI.QueryModuleResources('', LQuery); // Search all modules
    if LResponse.TryGetArray(LJSONArray) then
      for LJSONValue in LJSONArray do
        if LJSONValue.TryGetValue<string>('resourcename', LResourceName) then
        begin
          LModuleName := LJSONValue.GetValue<string>('modulename');
          LModuleNames := LModuleNames + [LModuleName];
        end;
  finally
    LAPI.Free;
  end;
  Result := LModuleNames;
end;


procedure Register;
begin
  RegisterResource(TypeInfo(TMeasurementsResource));
end;

initialization
  Register;

end.
