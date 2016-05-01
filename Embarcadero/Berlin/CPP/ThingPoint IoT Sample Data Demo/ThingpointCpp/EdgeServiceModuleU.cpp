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

#include "EdgeServiceModuleU.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"
TEdgeServiceModule *EdgeServiceModule;

// ---------------------------------------------------------------------------
__fastcall TEdgeServiceModule::TEdgeServiceModule(TComponent* Owner)
    : TDataModule(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TEdgeServiceModule::EMSEdgeService1Registered(TObject *Sender) {
    TEMSEdgeService* LEdgeService = (TEMSEdgeService*)Sender;
    _di_IGetEMSApi LGetEMSAPI;
    Supports(LEdgeService->ProviderService, __uuidof(IGetEMSApi), &LGetEMSAPI);
    assert(LGetEMSAPI);
    TEMSClientAPI * LAPI = LGetEMSAPI->EMSAPI;

    struct TQueryModuleNameHandler
	: public TCppInterfacedObject<TEMSClientAPI::TQueryModuleNameProc> {
	String FProtocolProps;

	virtual void __fastcall Invoke(const TEMSClientAPI::TModule &AModule,
	    System::Json::TJSONObject* const AObj) {
	    FProtocolProps = AModule.ProtocolProps;
	}
    };

    TQueryModuleNameHandler* LQueryModuleNameHandler =
	new TQueryModuleNameHandler();
    TEMSClientAPI::_di_TQueryModuleNameProc callback = LQueryModuleNameHandler;

    // Get this module
    LAPI->QueryModuleName(LEdgeService->ModuleName, callback);

    // Get all other modules
    String LQuery[1];
    LQuery[0] = String::Format("where={\"modulename\":{\"$ne\":\"%s\"}}",
	ARRAYOFCONST((LEdgeService->ModuleName)));
    System::DynamicArray<TEMSClientAPI::TModule>LModules;
    LAPI->QueryModules(EXISTINGARRAY(LQuery), NULL, LModules);
    int length = LModules.Length;
    for (int i = 0; i < LModules.Length; i++) {
	TEMSClientAPI::TModule & LModule = LModules[i];
	if (SameText(LModule.ProtocolProps,
	    LQueryModuleNameHandler->FProtocolProps)) {
	    bool LDelete = false;
	    if (FOnProtocolPropsConflictDelete != NULL)
		FOnProtocolPropsConflictDelete(LModule.ModuleName,
		LModule.ProtocolProps, LDelete);

	    if (LDelete) {
		LAPI->UnregisterModule(LModule.ModuleID);
	    }
	}
    }
}

// ---------------------------------------------------------------------------
void __fastcall TEdgeServiceModule::EMSEdgeService1Registering(TObject *Sender,
    TJSONObject * const AModuleDetail, TJSONArray * const AResources,
    bool &AHandled)

{
    TEMSEdgeService* LEdgeService = (TEMSEdgeService*)Sender;
    _di_IGetEMSApi LGetEMSAPI;
    Supports(LEdgeService->ProviderService, __uuidof(IGetEMSApi), &LGetEMSAPI);
    assert(LGetEMSAPI);
    TEMSClientAPI * LAPI = LGetEMSAPI->EMSAPI;

    struct TQueryModuleNameHandler
	: public TCppInterfacedObject<TEMSClientAPI::TQueryModuleNameProc> {
	String FModuleID;

	virtual void __fastcall Invoke(const TEMSClientAPI::TModule &AModule,
	    System::Json::TJSONObject* const AObj) {
	    FModuleID = AModule.ModuleID;
	}
    };

    TQueryModuleNameHandler* LQueryModuleNameHandler =
	new TQueryModuleNameHandler();
    TEMSClientAPI::_di_TQueryModuleNameProc callback = LQueryModuleNameHandler;

    // Get this module
    LAPI->QueryModuleName(LEdgeService->ModuleName, callback);

    if (LQueryModuleNameHandler->FModuleID != "") {
	bool LDelete = false;
	if (FOnModuleOverwrite != NULL)
	    FOnModuleOverwrite(LEdgeService->ModuleName, LDelete);

	if (LDelete) {
	    // Prompt to replace existing module
	    LAPI->UnregisterModule(LQueryModuleNameHandler->FModuleID);
	}
    }

}
// ---------------------------------------------------------------------------
