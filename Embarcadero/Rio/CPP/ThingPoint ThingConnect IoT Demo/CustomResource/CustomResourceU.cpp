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
#pragma hdrstop

#include "CustomResourceU.h"
#include "System.Generics.Collections.hpp"
#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "System.Classes.TPersistent"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TMeasurementsResource::TMeasurementsResource(TComponent* Owner)
	: TDataModule(Owner)
{
}

void TMeasurementsResource::Get(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	std::vector<String> moduleNames;
	GetModuleNames(AContext, moduleNames);
	std::unique_ptr<TEMSInternalAPI> lApi(new TEMSInternalAPI(AContext));
	std::vector<String>::iterator it;
        std::unique_ptr<TJSONArray> lResultArray(new TJSONArray());
	for(it = moduleNames.begin(); it != moduleNames.end(); it++) {
		try{
			_di_IEMSResourceResponseContent lResponse = lApi->ModuleResourceGet(*it, "Measurements",
				DynamicArray<System::UnicodeString>(), DynamicArray<TPair__2<System::UnicodeString,System::UnicodeString> >());
                        TJSONArray * lResponseArray;
			if(lResponse->TryGetArray(lResponseArray)) {
				std::unique_ptr<TJSONObject> lResultObject(new TJSONObject());
				lResultObject->AddPair("edge", new TJSONString(*it));
				lResultObject->AddPair("data", (TJSONArray*)lResponseArray->Clone());
				lResultArray->AddElement(lResultObject.release());
			}
		}
		catch(System::Sysutils::Exception &ex) {
			// Error contacting edgemodule
                        std::unique_ptr<TJSONObject> lResultObject(new TJSONObject());
			lResultObject->AddPair("edge", new TJSONString(*it));
			lResultObject->AddPair("error", new TJSONString(ex.Message));
			lResultArray->AddElement(lResultObject.release());
		}
	}
        AResponse->Body->SetValue(lResultArray.release(), true);
}

void __fastcall TMeasurementsResource::GetModuleNames(TEndpointContext * AContext, std::vector<String> & names)
{
	TEMSInternalAPI * lApi = new TEMSInternalAPI(AContext);
        TEMSInternalAPI::TQueryParam lQueryParam;
        lQueryParam.Key = "where";
        lQueryParam.Value = "{\"resourcename\":\"Measurements\"}";
        DynamicArray<TEMSInternalAPI::TQueryParam> params;
        params.set_length(1);
        params[0] = lQueryParam;

	_di_IEMSResourceResponseContent response = lApi->QueryModuleResources("", params);
	TJSONArray * AJSONArray = response->GetArray();
	for(int i = 0; i < AJSONArray->Count; i++) {
	   String moduleName = static_cast<TJSONObject*>(AJSONArray->Items[i])->Get("modulename")->JsonValue->Value();
	   names.push_back(moduleName);
	}
}

static void Register()
{
	std::auto_ptr<TEMSResourceAttributes> attributes(new TEMSResourceAttributes());
	attributes->ResourceName = "Measurements";
	RegisterResource(__typeinfo(TMeasurementsResource), attributes.release());
}

#pragma startup Register 32


