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

#include "StreamingDeviceFrameU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TStreamingDeviceFrame *StreamingDeviceFrame;
//---------------------------------------------------------------------------
__fastcall TStreamingDeviceFrame::TStreamingDeviceFrame(TComponent* Owner)
	: TFrame(Owner)
{
  FValue = new TJSONObject();
}
//---------------------------------------------------------------------------
void __fastcall TStreamingDeviceFrame::ActionStartStopExecute(TObject * Sender)
{
  if (Started)
    Stop();
  else
    Start();
}

void __fastcall TStreamingDeviceFrame::ActionStartStopUpdate(TObject * Sender)
{
  if (Started)
    ((TAction*)Sender)->Text = "Stop";
  else
    ((TAction*)Sender)->Text = "Start";
}

void __fastcall TStreamingDeviceFrame::ButtonClearClick(TObject * Sender)
{
  DoClear();
}

void __fastcall TStreamingDeviceFrame::ClearLog(void)
{
  Memo1->Lines->Clear();
}

void __fastcall TStreamingDeviceFrame::DoClear(void)
{
  if (FOnClear != NULL)
    FOnClear(this);
}

__fastcall TStreamingDeviceFrame::~TStreamingDeviceFrame()
{
  delete FValue;
}

void __fastcall TStreamingDeviceFrame::DoChanged(void)
{
  if (FOnChanged != NULL)
    FOnChanged(this);
}

void __fastcall TStreamingDeviceFrame::DoNextValue(void)
{
  if (this->FOnNextValue != NULL)
  {
    while (FValue->Count > 0)
      FValue->RemovePair(FValue->Pairs[0]->JsonString->Value());
    FOnNextValue(FValue);
  }
}

bool __fastcall TStreamingDeviceFrame::GetStarted(void)
{
  return Timer1->Enabled;
}

void __fastcall TStreamingDeviceFrame::LogValue(void)
{
  Memo1->Lines->Insert(0, String::Format("[%s] %s", ARRAYOFCONST((FormatDateTime("mm:ss:zzz", FTime), Value->ToString()))));
}

void __fastcall TStreamingDeviceFrame::NextValue(void)
{
  DoNextValue();
  FTime = Now();
}

void __fastcall TStreamingDeviceFrame::SetStarted(bool Value)
{
  Timer1->Enabled = Value;
}

void __fastcall TStreamingDeviceFrame::Start(void)
{
  Timer1->Enabled = true;
}

void __fastcall TStreamingDeviceFrame::Stop(void)
{
  Timer1->Enabled = false;
}

void __fastcall TStreamingDeviceFrame::Timer1Timer(TObject * Sender)
{
  NextValue();
  DoChanged();
}
