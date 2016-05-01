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

#include "EndpointResultFrameU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TEMSEndpointResultFrame *EMSEndpointResultFrame;
//---------------------------------------------------------------------------
__fastcall TEMSEndpointResultFrame::TEMSEndpointResultFrame(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEMSEndpointResultFrame::Notification(TComponent * AComponent, TOperation Operation)
{
	TControl::Notification(AComponent, Operation);
	if(Operation == opRemove) {
		if(FBackendEndpoint == AComponent) {
			FBackendEndpoint = NULL;
		}
	}
}

void __fastcall TEMSEndpointResultFrame::CheckBackendEndpoint(void)
{
	if(FBackendEndpoint == NULL) {
        throw System::Sysutils::Exception("Missing BackendEndpoint");
    }
}

void __fastcall TEMSEndpointResultFrame::SetBackendEndpoint(const TBackendEndpoint * value)
{
	if(FBackendEndpoint != value) {
		if(FBackendEndpoint != NULL) {
			FBackendEndpoint->RemoveFreeNotification(this);
		}
		FBackendEndpoint = const_cast<TBackendEndpoint*>(value);
		if(FBackendEndpoint != NULL) {
			FBackendEndpoint->FreeNotification(this);
		}
		BackendEndpointChanged();
	}
}

void __fastcall TEMSEndpointResultFrame::BackendEndpointChanged(void)
{
	if(FBackendEndpoint != NULL) {
		RESTResponseDataSetAdapter1->ResponseJSON = FBackendEndpoint->operator _di_IRESTResponseJSON();
	}
	else {
		RESTResponseDataSetAdapter1->ResponseJSON = NULL;
	}
}

void __fastcall TEMSEndpointResultFrame::SetSettingsList(const TSettingsList * value)
{
	FSettingsList = const_cast<TSettingsList*>(value);
	RestoreGridColumnWidths();
}

String __fastcall TEMSEndpointResultFrame::MakeWidthKey(const String AHeader)
{
	String _return = this->Name + "||" + AHeader;
	int i = 0;
	do {
		i = PosEx("[", _return);
		if(i >= 0) {
			int j = PosEx("]", _return, i);
			if(j > 0) {
				_return = _return.SubString(0, i) + "||" + _return.SubString(j+1, _return.Length());
			} else {
				_return = _return.SubString(0, i) + "||";
			}
		}
	}
	while(i < 0);
	if(_return.SubString(0,3) == "||."){
		_return = _return.SubString(3, _return.Length()-3);
	}
	return _return;
}

void __fastcall TEMSEndpointResultFrame::RestoreGridColumnWidths(void)
{
	for(int i = 0; i < StringGrid1->ColumnCount; i++) {
		TColumn * col = StringGrid1->Columns[i];
		String sKey = MakeWidthKey(col->Header);
		int iWidth = 0;
		if(FSettingsList->GetWidth(sKey, iWidth)) {
			col->Width = iWidth;
		}
	}
}

void __fastcall TEMSEndpointResultFrame::SaveGridColumnWidths(void)
{
	for(int i = 0; i < StringGrid1->ColumnCount; i++) {
		TColumn * col = StringGrid1->Columns[i];
		String sKey = MakeWidthKey(col->Header);
		FSettingsList->AddWidth(sKey, static_cast<int>(col->Width));
	}
}

bool __fastcall TEMSEndpointResultFrame::GetEmpty(void)
{
	return (StringGrid1->RowCount == 0);
}

bool __fastcall TEMSEndpointResultFrame::GetAutoRefresh(void)
{
	return Timer1->Enabled;
}

void __fastcall TEMSEndpointResultFrame::SetAutoRefresh(const bool value)
{
	if(value) {
		CheckBackendEndpoint();
	}
	Timer1->Enabled = value;
}

void __fastcall TEMSEndpointResultFrame::Execute(void)
{
	CheckBackendEndpoint();
	SaveGridColumnWidths();
	BackendEndpoint->Execute();
	RestoreGridColumnWidths();
}

void __fastcall TEMSEndpointResultFrame::Clear(void)
{
	CheckBackendEndpoint();
	AutoRefresh = false;
	SaveGridColumnWidths();
	if (BackendEndpoint->Response != NULL) {
		BackendEndpoint->Response->ResetToDefaults();
	}
}

void __fastcall TEMSEndpointResultFrame::SaveSettings(void)
{
	SaveGridColumnWidths();
}
void __fastcall TEMSEndpointResultFrame::ButtonExecuteClick(TObject *Sender)
{
	Execute();
}
//---------------------------------------------------------------------------

void __fastcall TEMSEndpointResultFrame::ButtonClearClick(TObject *Sender)
{
	Clear();
}
//---------------------------------------------------------------------------

void __fastcall TEMSEndpointResultFrame::CheckBoxAutoRefreshClick(TObject *Sender)
{
	AutoRefresh = !AutoRefresh;
}
//---------------------------------------------------------------------------

void __fastcall TEMSEndpointResultFrame::Timer1Timer(TObject *Sender)
{
	try {
		Execute();
	}
	catch (...) {
		Timer1->Enabled  = false;
		throw;
	}
}
//---------------------------------------------------------------------------

void __fastcall TEMSEndpointResultFrame::ActionAutoRefreshExecute(TObject *Sender)
{
	AutoRefresh = !AutoRefresh;
}
//---------------------------------------------------------------------------

void __fastcall TEMSEndpointResultFrame::ActionAutoRefreshUpdate(TObject *Sender)

{
	static_cast<TAction*>(Sender)->Checked = AutoRefresh;
}
//---------------------------------------------------------------------------

