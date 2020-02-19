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
unit EdgeModuleResourceU;

// EMS Resource Unit

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI,
  EMS.ResourceTypes;

type
  [ResourceName('Measurements')]
{$METHODINFO ON}
  TMeasurementsResource = class
  private type
    TCallback = reference to function(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean;
  private
    procedure DoGetResponse(const AResponse: TEndpointResponse; const ACallback: TCallback);
  published

    procedure GetSimple(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('detailed')]
    procedure GetDetailed(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;
{$METHODINFO OFF}

implementation

uses CacheDataModuleU, System.DateUtils;

procedure TMeasurementsResource.DoGetResponse(const AResponse: TEndpointResponse; const ACallback: TCallback);
var
  LDeviceData: TJSONObject;
  LResultData: TJSONObject;
  LJSONArray: TJSONArray;
  LDevices: TArray<string>;
  LDevice: string;
  LTime: TDateTime;
begin
  LDevices := TArray<string>.Create('heartrate', 'bloodpressure');
  LJSONArray := TJSONArray.Create;
  LResultData := nil;
  LDeviceData := nil;
  try
    for LDevice in LDevices do
    begin
      LDeviceData := TJSONObject.Create;
      if ACallback(LDevice, LTime, LDeviceData) then
      begin
        LResultData := TJSONObject.Create;
        LResultData.AddPair('device', LDevice);
        LResultData.AddPair('time', DateToISO8601(LTime));;
        LResultData.AddPair('data', LDeviceData);
        LDeviceData := nil;
        LJSONArray.Add(LResultData);
        LResultData := nil;
      end;
    end;
    AResponse.Body.SetValue(LJSONArray, True);
    LJSONArray := nil;
  finally
    LJSONArray.Free;
    LDeviceData.Free;
    LResultData.Free;
  end;

end;


procedure TMeasurementsResource.GetSimple(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  DoGetResponse(AResponse,
    function(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean
    begin
      Result := CacheDataModule.TryGetRecentDeviceData(ADevice, ATime, AData);
    end);
end;

procedure TMeasurementsResource.GetDetailed(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  DoGetResponse(AResponse,
    function(const ADevice: string; out ATime: TDateTime; const AData: TJSONObject): Boolean
    begin
      Result := CacheDataModule.TryGetDeviceDataStats(ADevice, ATime, AData);
    end);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TMeasurementsResource));
end;

initialization

Register;

end.
