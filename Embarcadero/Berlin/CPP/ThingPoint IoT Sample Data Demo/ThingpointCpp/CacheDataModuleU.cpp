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

#include <System.JSON.hpp>
#include <System.Math.hpp>
#pragma hdrstop
#include "CacheDataModuleU.h"
#include <memory>
#include <map>
#include <vector>
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"

TCacheDataModule *CacheDataModule;

// ---------------------------------------------------------------------------
__fastcall TCacheDataModule::TCacheDataModule(TComponent* Owner)
    : TDataModule(Owner) {
    FCache = new TCache();
}

__fastcall TCacheDataModule::~TCacheDataModule(void) {
    delete FCache;

}

void __fastcall TCacheDataModule::SaveDeviceData(String ADevice,
    TDateTime ATime, TJSONObject * AData) {
    TItem* LItem = new TItem(ATime, ADevice, "", AData);
    FCache->Save(LItem);
}

void __fastcall TCacheDataModule::ClearDeviceData(String ADevice) {
    FCache->ClearDevice(ADevice);

}

bool __fastcall TCacheDataModule::TryGetRecentDeviceData(String ADevice,
    TDateTime &ATime, TJSONObject * AData) {
    TItem AItem;
    if (FCache->TryGetRecent(ADevice, AItem)) {
	ATime = AItem.Time;
	std::auto_ptr<TJSONObject>LJSON
	    (dynamic_cast<TJSONObject*>(TJSONObject::ParseJSONValue
	    (AItem.Value)));
	if (LJSON.get() != NULL) {

	    for (int i = 0; i < LJSON->Count; i++) {
		AData->AddPair
		    (dynamic_cast<TJSONPair*>(LJSON->Pairs[i]->Clone()));
	    }
	}
	return true;
    }
    else
	return false;
}

typedef System::DynamicArray<double>TDoublesArray;

class Stats {
public:
    TDateTime FTime;
    bool FFound;

    std::map<String, TDoublesArray>FMap;

    Stats::Stats() {
	FFound = false;
    }

    void __fastcall OnItem(const TItem &AItem, bool &ADone) {
	if (!FFound)
	    FTime = AItem.Time;
	FFound = true; // Found
	std::auto_ptr<TJSONObject>LJSON
	    (dynamic_cast<TJSONObject*>(TJSONObject::ParseJSONValue
	    (AItem.Value)));
	assert(LJSON.get() != NULL);
	for (int i = 0; i < LJSON->Count; i++) {
	    TJSONNumber * LJSONNumber =
		(dynamic_cast<TJSONNumber*>(LJSON->Pairs[i]->JsonValue));
	    if (LJSONNumber != NULL) {
		String key = LJSON->Pairs[i]->JsonString->Value();
		TDoublesArray &array = FMap[key];
		array.set_length(array.Length + 1);
		array[array.Length - 1] = LJSONNumber->AsDouble;
	    }
	}
    }
};

bool __fastcall TCacheDataModule::TryGetDeviceDataStats(String ADevice,
    TDateTime &ATime, TJSONObject *AData) {
    std::auto_ptr<Stats>LStats(new Stats());

    FCache->EnumItems(ADevice, LStats->OnItem);
    if (LStats->FFound) {
	ATime = LStats->FTime;
	std::map<String, TDoublesArray>::iterator it;
	for (it = LStats->FMap.begin(); it != LStats->FMap.end(); ++it) {
	    double LMean;
	    double LStdDev;
	    TDoublesArray &LList = it->second;
	    System::Math::MeanAndStdDev(&LList[0], LList.Length - 1, LMean,
		LStdDev);
	    std::auto_ptr<TJSONObject>LJSONObject(new TJSONObject());
	    LJSONObject->AddPair("last", new TJSONNumber(LList[0]));
	    LJSONObject->AddPair("mean", new TJSONNumber(LMean));
	    LJSONObject->AddPair("stddev", new TJSONNumber(LStdDev));
	    String key = it->first;
	    AData->AddPair(key, LJSONObject.release());
	}

    }
    return LStats->FFound;

}

bool __fastcall TCache::TryGetRecent(String ADevice, TItem &AItem) {
    std::vector<TItem>&v = *FList;
    std::vector<TItem>::iterator it;
    for (it = v.begin(); it < v.end(); it++) {
	if (ADevice == it->Device) {
	    AItem = *it;
	    return true;
	}
    }
    return false;
}

bool __fastcall TCache::EnumItems(String ADevice, TItemsEvent AOnItem) {
    std::vector<TItem>&v = *FList;
    std::vector<TItem>::iterator it;
    for (it = v.begin(); it < v.end(); it++) {
	if (ADevice == it->Device) {
	    bool LDone = false;
	    AOnItem(*it, LDone);
	    if (LDone) {
		return true;
	    }
	}
    }
    return false;
}

void __fastcall TCache::ClearDevice(String ADevice) {
    std::vector<TItem>&v = *FList;
    std::vector<TItem>::iterator it;
    for (it = v.begin(); it < v.end(); it++) {
	if (ADevice == it->Device) {
	    std::vector<TItem>::iterator del = it;
	    it++;
	    v.erase(del);
	}
    }
}

void __fastcall TCache::Save(TItem * AItem) {
    bool append = true;
    std::vector<TItem>&v = *FList;
    std::vector<TItem>::iterator it;
    for (it = v.begin(); it < v.end(); it++) {
	if (AItem->Time > it->Time) {
	    append = false;
	    v.insert(it, *AItem);
	    break;
	}
    }
    if (append) {
	v.push_back(*AItem);
    }
}

// ---------------------------------------------------------------------------
