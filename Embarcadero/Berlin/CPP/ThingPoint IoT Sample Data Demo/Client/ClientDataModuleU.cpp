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

#include <memory>
#include "ClientDataModuleU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"
TEMSClientDataModule *EMSClientDataModule;
//---------------------------------------------------------------------------
__fastcall TEMSClientDataModule::TEMSClientDataModule(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
String __fastcall TEMSClientDataModule::GetCurrentEdge(void) {
	return BackendEndpointEdgeMeasurements->Params->Items[0]->Value;
}

void __fastcall TEMSClientDataModule::GetEdgeNames(std::vector<String> & value)
{
	BackendQueryEdgeModules->Execute();
	TJSONArray * jArray =  BackendQueryEdgeModules->JSONResult;
	if(jArray != NULL) {
		for(int i = 0; i < jArray->Count; i++) {
			String moduleName = static_cast<TJSONObject*>(jArray->Items[i])->Get("modulename")->JsonValue->Value();
			value.push_back(moduleName);
        }
    }
}

void __fastcall TEMSClientDataModule::SetCurrentEdge(const String Value)
{
	BackendEndpointEdgeMeasurements->Params->Items[0]->Value = Value;
	BackenEndpointEdgeDetailedMeasurements->Params->Items[0]->Value = Value;
}
