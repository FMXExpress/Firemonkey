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

#include "NotifyDeviceFrameU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TNotifyDeviceFrame *NotifyDeviceFrame;
//---------------------------------------------------------------------------
__fastcall TNotifyDeviceFrame::TNotifyDeviceFrame(TComponent* Owner)
	: TFrame(Owner)
{
  FValue = new TJSONObject();
}
//---------------------------------------------------------------------------
void __fastcall TNotifyDeviceFrame::ActionStartStopExecute(TObject * Sender)
{
  NextValue();
  DoChanged();
}

void __fastcall TNotifyDeviceFrame::ButtonClearClick(TObject * Sender)
{
  DoClear();
}

void __fastcall TNotifyDeviceFrame::ClearLog(void)
{
  Memo1->Lines->Clear();
}

void __fastcall TNotifyDeviceFrame::DoClear(void)
{
  if (FOnClear != NULL)
    FOnClear(this);
}

__fastcall TNotifyDeviceFrame::~TNotifyDeviceFrame()
{
  delete FValue;
}

void __fastcall TNotifyDeviceFrame::DoChanged(void)
{
  if (FOnChanged != NULL)
    FOnChanged(this);
}

void __fastcall TNotifyDeviceFrame::DoNextValue(void)
{
  if (this->FOnNextValue != NULL)
  {
    while (FValue->Count > 0)
      FValue->RemovePair(FValue->Pairs[0]->JsonString->Value());
    FOnNextValue(FValue);
  }
}


void __fastcall TNotifyDeviceFrame::LogValue(void)
{
  Memo1->Lines->Insert(0, String::Format("[%s] %s", ARRAYOFCONST((FormatDateTime("mm:ss:zzz", FTime), Value->ToString()))));
}

void __fastcall TNotifyDeviceFrame::NextValue(void)
{
  DoNextValue();
  FTime = Now();
}


