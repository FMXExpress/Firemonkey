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

#include <System.JSON.hpp>
#include <System.SysUtils.hpp>
#include <memory>
#include "ClientSettingsU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

const String WIDTH =  "width";
const String WIDTHKEY = "widthkey";

__fastcall TSettingsList::TSettingsList(String AFileName)
{
	FFileName = AFileName;
	if( (FFileName != "") && FileExists(FFileName) ){
		LoadFromFile(FFileName);
	}
}

void __fastcall TSettingsList::Clear(void)
{
	FWidths.clear();
}

void __fastcall TSettingsList::AddWidth(String AKey, int AWidth)
{
	FWidths.insert(std::pair<String, int>(AKey, AWidth));
}

bool __fastcall TSettingsList::GetWidth(String AKey, int &AWidth)
{
	std::map<String,int>::iterator it = FWidths.find(AKey);
	if(it != FWidths.end()) {
		AWidth = it->second;
		return true;
	} else {
		return false;
    }
}

void __fastcall TSettingsList::SaveToFile(String AFileName)
{
	if(AFileName == "") {
		return;
	}
	std::unique_ptr<TJSONArray> lRoot(new TJSONArray());
	std::map<String, int>::iterator it;
	for(it = FWidths.begin(); it != FWidths.end(); it++) {
		TJSONObject * row = new TJSONObject();
		row->AddPair(WIDTHKEY, new TJSONString(it->first));
		row->AddPair(WIDTH, new TJSONNumber(it->second));
		lRoot->AddElement(row);
	}
	std::unique_ptr<TStringStream> lStream(new TStringStream(lRoot->ToString()));
	lStream->SaveToFile(AFileName);
}

void __fastcall TSettingsList::LoadFromFile(String AFileName)
{
	this->Clear();
	if(AFileName == "" || !FileExists(AFileName)) {
		return;
	}

	std::unique_ptr<TStringStream> lStream(new TStringStream(AFileName));
	lStream->LoadFromFile(AFileName);
	std::unique_ptr<TJSONArray> lRoot(static_cast<TJSONArray*>(TJSONObject::ParseJSONValue(lStream->DataString)));
	for(int i = 0; i < lRoot->Count; i++) {
		TJSONObject * row = static_cast<TJSONObject*>(lRoot->Items[i]);
		String sKey = static_cast<TJSONString*>(row->GetValue(WIDTHKEY))->Value();
		int iWidth = static_cast<TJSONNumber*>(row->GetValue(WIDTH))->AsInt;
		FWidths.insert(std::pair<String,int>(sKey, iWidth));
    }
}
