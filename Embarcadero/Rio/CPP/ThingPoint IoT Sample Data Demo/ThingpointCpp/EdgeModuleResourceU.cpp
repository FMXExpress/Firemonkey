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
#include <System.DateUtils.hpp>
#pragma hdrstop

#include "EdgeModuleResourceU.h"
#include <memory>
#include <vector>
// ---------------------------------------------------------------------------
#pragma package(smart_init)
// ---------------------------------------------------------------------------

void __fastcall TMeasurementsResource::DoGetResponse
    (TEndpointResponse * AResponse, TMeasurementsEvent ACallback) {
    std::auto_ptr<TJSONArray>LJSONArray(new TJSONArray());
    std::vector<String> LDevices;
    LDevices.push_back("heartrate");
    LDevices.push_back("bloodpressure");
    for (std::vector<String>::iterator it=LDevices.begin(); it<LDevices.end(); it++) {
	String &LDevice = *it;
	std::auto_ptr<TJSONObject>LDeviceData(new TJSONObject());
	bool LResult = false;
	TDateTime LTime;
	ACallback(LDevice, LTime, LDeviceData.get(), LResult);
	if (LResult) {
	    std::auto_ptr<TJSONObject>LResultData(new TJSONObject());
	    LResultData->AddPair("device", LDevice);
	    LResultData->AddPair("time", DateToISO8601(LTime));
	    LResultData->AddPair("data", LDeviceData.release());
	    LJSONArray->Add(LResultData.release());

	}

    }
    AResponse->Body->SetValue(LJSONArray.release(), true);
}

void __fastcall TMeasurementsResource::OnGetSimple(String ADevice,
    TDateTime &ATime, TJSONObject * AData, bool &AResult) {
    AResult = CacheDataModule->TryGetRecentDeviceData(ADevice, ATime, AData);
}

void __fastcall TMeasurementsResource::OnGetDetailed(String ADevice,
    TDateTime &ATime, TJSONObject * AData, bool &AResult) {
    AResult = CacheDataModule->TryGetDeviceDataStats(ADevice, ATime, AData);
}

void TMeasurementsResource::GetSimple(TEndpointContext* AContext,
    TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    DoGetResponse(AResponse, OnGetSimple);
}

void TMeasurementsResource::GetDetailed(TEndpointContext* AContext,
    TEndpointRequest* ARequest, TEndpointResponse* AResponse) {
    DoGetResponse(AResponse, OnGetDetailed);
}

static void Register() {
    std::auto_ptr<TEMSResourceAttributes>attributes
	(new TEMSResourceAttributes());
    attributes->ResourceName = "Measurements";
    attributes->ResourceSuffix["GetDetailed"] = "detailed";
    RegisterResource(__typeinfo(TMeasurementsResource), attributes.release());
}

#pragma startup Register 32
