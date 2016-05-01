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
#pragma link "EndpointResultFrameU"
#pragma resource "*.fmx"
TEMSThingPointForm *EMSThingPointForm;
//---------------------------------------------------------------------------
__fastcall TEMSThingPointForm::TEMSThingPointForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEMSThingPointForm::FormCreate(TObject *Sender)
{
	FSettingsList = new TSettingsList(SETTINGSDBFILE);
	EMSServerConnectionFrame1->EMSProvider = EMSClientDataModule->EMSProvider1;
	MeasurementsResultFrame->BackendEndpoint = EMSClientDataModule->BackendEndpointMeasurements;
	MeasurementsResultFrame->SettingsList = FSettingsList;
	EdgeResultsFrame->BackendEndpoint = EMSClientDataModule->BackendEndpointEdgeMeasurements;
	EdgeResultsFrame->SettingsList = FSettingsList;
	Application->OnIdle = &OnIdle;
	TabControl1->ActiveTab = TabItemServerRequest; // First tab
}
//---------------------------------------------------------------------------
void __fastcall TEMSThingPointForm::OnIdle(TObject * Sender, bool &ADone)
{
	EdgeResultsFrame->ButtonExecute->Enabled = (EMSClientDataModule->CurrentEdge != EmptyStr);
}

void __fastcall TEMSThingPointForm::RefreshEdgeNames(void)
{
	String sSave = EMSClientDataModule->CurrentEdge;
	TStrings * sItems = ComboBoxEdgeName->Items;
	sItems->BeginUpdate();
	try
    {
		sItems->Clear();
		std::vector<String> eNames;
		EMSClientDataModule->GetEdgeNames(eNames);
		for(std::vector<String>::iterator it = eNames.begin(); it != eNames.end(); it++) {
			sItems->Add(*it);
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
	ComboBoxEdgeName->ItemIndex = iIndex;
	ComboBoxEdgeNameChange(this);
}

void __fastcall TEMSThingPointForm::FormClose(TObject *Sender, TCloseAction &Action)

{
	EdgeResultsFrame->SaveSettings();
	MeasurementsResultFrame->SaveSettings();
	FSettingsList->SaveToFile();
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::FormDestroy(TObject *Sender)
{
	delete FSettingsList;
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::MeasurementsResultFrameButtonExecuteClick(TObject *Sender)
{
	MeasurementsResultFrame->ButtonExecuteClick(Sender);
	if (MeasurementsResultFrame->Empty){
		ShowMessage("No data");
	}
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::EMSServerConnectionFrame1ButtonTestConnectionClick(TObject *Sender)
{
	EMSServerConnectionFrame1->ButtonTestConnectionClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::MeasurementsResultFrameButtonClearClick(TObject *Sender)
{
	MeasurementsResultFrame->ButtonClearClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::ComboBoxEdgeNameChange(TObject *Sender)
{
	String sEdge;
	if(ComboBoxEdgeName->Selected != NULL) {
		sEdge = ComboBoxEdgeName->Selected->Text;
	}
	if(sEdge != EMSClientDataModule->CurrentEdge) {
		EMSClientDataModule->CurrentEdge = sEdge;
		EdgeResultsFrame->Clear();
    }
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::ButtonRefreshEdgeNamesClick(TObject *Sender)
{
	RefreshEdgeNames();
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::EdgeResultsFrameButtonExecuteClick(TObject *Sender)
{
	EdgeResultsFrame->ButtonExecuteClick(Sender);
	if(EdgeResultsFrame->Empty) {
		ShowMessage("No data");
	}
}
//---------------------------------------------------------------------------

void __fastcall TEMSThingPointForm::TabItemEdgeRequestClick(TObject *Sender)
{
	RefreshEdgeNames();
}
//---------------------------------------------------------------------------

