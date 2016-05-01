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
#include <fmx.h>
#pragma hdrstop

#include "MainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ConnectionFrameU"
#pragma resource "*.fmx"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ShowSettings(void)
{
	TabControl1->ActiveTab = TabItemSettings;
}

void __fastcall TMainForm::ShowThings(void)
{
	if (ListBox1->Items->Count == 0)
		ListThings();
	if (ListBox1->Items->Count == 0) {
		ShowMessage("No Things found");
	}
	else {
		TabControl1->ActiveTab = TabItemThings;
	}
}

void __fastcall TMainForm::UpdateMultiview(void)
{
	bool bShowingThigs = (TabControl1->ActiveTab == TabItemThings);
	if(bShowingThigs) {
		MultiView1->Visible = true;
		MultiView1->MasterButton = MasterButton;
		MasterButton->Visible = true;
	}
	else {
		MultiView1->Visible = false;
		MultiView1->MasterButton = NULL;
		MasterButton->Visible = false;
	}
}

void __fastcall TMainForm::ListThings(void)
{
	String sSave = EMSClientDataModule->CurrentEdge;
	TStrings * sItems = ListBox1->Items;
	sItems->BeginUpdate();
	try
    {
		sItems->Clear();
		std::vector<String> sEdgeNames;
		EMSClientDataModule->GetEdgeNames(sEdgeNames);
		std::vector<String>::iterator it;
		for(it = sEdgeNames.begin(); it != sEdgeNames.end(); it++) {
			sItems->Add(*it);
		}
		if(sItems->Count > 0) {
			sItems->Insert(0, "All");
		}
	}
	__finally
	{
		sItems->EndUpdate();
	}
	int iIndex = sItems->IndexOf(sSave);
	if((iIndex < 0) && (sItems->Count > 0)) {
		iIndex = 0;
	}
	ListBox1->ItemIndex = iIndex;
	ListBox1Change(this);
}

void __fastcall TMainForm::ListObject(const String APrefix, const TJSONObject * AJSONObject)
{
	TJSONObject * obj = const_cast<TJSONObject*>(AJSONObject);
	for(int i = 0; obj->Count; i++) {
		TJSONPair * jPair = obj->Pairs[i];
		if(static_cast<TJSONObject*>(jPair->JsonValue) != NULL) {
			ListObject(jPair->JsonString->Value(), static_cast<TJSONObject*>(jPair->JsonValue));
		}
		else {
			TListViewItem * item = ListView1->Items->Add();
			if(APrefix != "") {
				item->Text = APrefix + "." + jPair->JsonString->Value();
			}
			else {
				item->Text = jPair->JsonString->Value();
			}
			if(static_cast<TJSONNumber*>(jPair->JsonValue) != NULL) {
				item->Detail = FormatFloat(".##", (static_cast<TJSONNumber*>(jPair->JsonValue))->AsDouble);
			}
			else {
                item->Detail = jPair->JsonValue->Value();
            }
        }
    }
}

void __fastcall TMainForm::RefreshThing(void)
{
	ListView1->BeginUpdate();
	try
	{
		ListView1->Items->Clear();
		if(EMSClientDataModule->CurrentEdge != EmptyStr) {
			TJSONArray * jArray = ExecuteEndpoint();
			for(int i = 0; i < jArray->Count; i++) {
				TJSONObject * jObj = static_cast<TJSONObject*>(jArray->Items[i]);
				String sEdge = jObj->Get("edge")->JsonValue->Value();
				if(sEdge != EmptyStr) {
					// Summary by edge name
					TListViewItem * item = ListView1->Items->Add();
					item->Purpose = TListItemPurpose::Header;
					item->Text = sEdge;
				}
				TJSONArray * lDataArray = static_cast<TJSONArray*>(jObj->Get("data")->JsonValue);
				if(lDataArray != NULL){
					for(int j = 0; j < lDataArray->Count; j++) {
						TJSONObject * jObj2 = static_cast<TJSONObject*>(lDataArray->Items[j]);
						String sDevice = jObj2->Get("device")->JsonValue->Value();
						String sTime = jObj2->Get("time")->JsonValue->Value();
						TListViewItem * item = ListView1->Items->Add();
						item->Text = sDevice;
						item->Detail = sTime;
						if(static_cast<TJSONObject*>(jObj2->Get("data")->JsonValue) != NULL){
							ListObject(sDevice, static_cast<TJSONObject*>(jObj2->Get("data")->JsonValue));
						}
                    }
				} else {
					// Informatation about a single edge.  Edge may have multiple devices.
					String sDevice = jObj->Get("device")->JsonValue->Value();
					String sTime = jObj->Get("time")->JsonValue->Value();
                    TListViewItem * item = ListView1->Items->Add();
					item->Text = sDevice;
					item->Detail = sTime;
					if(static_cast<TJSONObject*>(jObj->Get("data")->JsonValue) != NULL){
						ListObject("", static_cast<TJSONObject*>(jObj->Get("data")->JsonValue));
					}
                }
            }
        }
	}
	 __finally
	{
		ListView1->EndUpdate();
	}
}

TJSONArray * __fastcall TMainForm::ExecuteEndpoint(void)
{
	TBackendEndpoint * endPoint = NULL;
	if(ShowingAll()) {
		 endPoint = EMSClientDataModule->BackendEndpointMeasurements;
	}
	else if(SwitchDetailed->IsChecked) {
		endPoint = EMSClientDataModule->BackenEndpointEdgeDetailedMeasurements;
	}
	else {
		endPoint = EMSClientDataModule->BackendEndpointEdgeMeasurements;
	}
	endPoint->Execute();
	return static_cast<TJSONArray*>(endPoint->Response->JSONValue);
}

bool __fastcall TMainForm::ShowingAll(void)
{
	return ((ListBox1->Count > 0) && (ListBox1->ItemIndex == 0));
}

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
	EMSServerConnectionFrame1->EMSProvider = EMSClientDataModule->EMSProvider1;
	ShowSettings();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ListBox1Change(TObject *Sender)
{
	String sEdge;
	if(ListBox1->Selected != NULL) {
		sEdge = ListBox1->Selected->Text;
	}
	if(sEdge != EMSClientDataModule->CurrentEdge) {
		EMSClientDataModule->CurrentEdge = sEdge;
		RefreshThing();
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item)
{
	MultiView1->HideMaster();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Timer1Timer(TObject *Sender)
{
	try {
		RefreshThing();
	} catch (...) {
		Timer1->Enabled = false;
		throw;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::SwitchDetailedSwitch(TObject *Sender)
{
	SetAutoRefresh(SwitchAutoRefresh->IsChecked);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SetAutoRefresh(bool AValue)
{
	Timer1->Enabled = AValue;
}

void __fastcall TMainForm::ActionBackExecute(TObject *Sender)
{
	ShowSettings();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionBackUpdate(TObject *Sender)
{
	TAction * action = static_cast<TAction*>(Sender);
	action->Text = "";
	action->Visible = TabControl1->ActiveTab != TabItemSettings;
	UpdateMultiview();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionForwardUpdate(TObject *Sender)
{
	TAction * act = static_cast<TAction*>(Sender);
	act->Text = "Things";
	act->Visible = TabControl1->ActiveTab != TabItemThings;
	UpdateMultiview();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionForwardExecute(TObject *Sender)
{
	ShowThings();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionCaptionUpdate(TObject *Sender)
{
	bool bShowing = TabControl1->ActiveTab == TabItemThings;
	if(bShowing){
		if(ShowingAll()){
			((TAction*)(Sender))->Text = "All things";
		}
		else if (EMSClientDataModule->CurrentEdge != "") {
            ((TAction*)(Sender))->Text = EMSClientDataModule->CurrentEdge;
		}
		else {
			((TAction*)(Sender))->Text = "(No Thing)";
        }
	}
	else {
        ((TAction*)(Sender))->Text = "Settings";
    }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionAutoRefreshExecute(TObject *Sender)
{
	SetAutoRefresh(((TAction*)(Sender))->Checked);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionAutoRefreshUpdate(TObject *Sender)
{
  ((TAction*)(Sender))->Enabled = (EMSClientDataModule->CurrentEdge != EmptyStr);
  ((TAction*)(Sender))->Checked = (((TAction*)(Sender))->Enabled && Timer1->Enabled);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionDetailedExecute(TObject *Sender)
{
	//
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActionDetailedUpdate(TObject *Sender)
{
	((TAction*)(Sender))->Enabled = !ShowingAll();
}
//---------------------------------------------------------------------------

