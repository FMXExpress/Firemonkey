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
#pragma link "ListenerFrameU"
#pragma link "LoggingFrameU"
#pragma link "StreamingDeviceFrameU"
#pragma link "NotifyDeviceFrameU"
#pragma resource "*.fmx"
TEdgeMainForm *EdgeMainForm;
//---------------------------------------------------------------------------
__fastcall TEdgeMainForm::TEdgeMainForm(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TEdgeMainForm::FormCreate(TObject *Sender)
{
  HeartRateFrame->OnChanged = HeartRateChanged;
  HeartRateFrame->OnClear = HeartRateClear;

  BloodPressureFrame->OnChanged = BloodPressureChanged;
  BloodPressureFrame->OnClear = BloodPressureClear;
  BloodPressureFrame->OnNextValue = BloodPressureNextValue;

  EdgeServiceModule->OnModuleOverwrite = OnModuleOverwrite;
  EdgeServiceModule->OnProtocolPropsConflictDelete =  OnProtocolPropsConflictDelete;

  EMSServerConnectionFrame1->EMSProvider = EdgeServiceModule->EMSProvider1;
  EMSEdgeModuleListenerFrame1->EMSEdgeService = EdgeServiceModule->EMSEdgeService1;
  EMSEdgeModuleListenerFrame1->EMSProvider = EdgeServiceModule->EMSProvider1;
  Application->OnIdle = OnIdle;

}

void __fastcall TEdgeMainForm::HeartRateChanged(TObject * Sender)
{
  HeartRateFrame->LogValue();
  CacheDataModule->SaveDeviceData("heartrate", HeartRateFrame->Time, HeartRateFrame->Value);
}

void __fastcall TEdgeMainForm::HeartRateClear(TObject * Sender)
{
  HeartRateFrame->ClearLog();
  CacheDataModule->ClearDeviceData("heartrate");
}

void __fastcall TEdgeMainForm::HeartRateNextValue(TJSONObject * Value)
{
  int LBeats = 68 + Random(5);
  Value->AddPair("bpm", new TJSONNumber(LBeats));
}

void __fastcall TEdgeMainForm::BloodPressureChanged(TObject * Sender)
{
  BloodPressureFrame->LogValue();
  CacheDataModule->SaveDeviceData("bloodpressure", BloodPressureFrame->Time, BloodPressureFrame->Value);
}

void __fastcall TEdgeMainForm::BloodPressureClear(TObject * Sender)
{
 BloodPressureFrame->ClearLog();
 CacheDataModule->ClearDeviceData("bloodpressure");
}

void __fastcall TEdgeMainForm::BloodPressureNextValue(TJSONObject * Value)
{
  int LPressure = 115 + Random(5);
   Value->AddPair("systolic", new TJSONNumber(LPressure));
  LPressure = 75 + Random(5);
  Value->AddPair("diastolic", new TJSONNumber(LPressure));
}

void __fastcall TEdgeMainForm::OnModuleOverwrite(String AModuleName, bool &AResult)
{
  int r = MessageDlg(Format("ModuleName '%0:s' is already registered.  Overwrite '%0:s?'",
        ARRAYOFCONST((AModuleName))),
        TMsgDlgType::mtConfirmation, TMsgDlgButtons() << TMsgDlgBtn::mbYes << TMsgDlgBtn::mbNo << TMsgDlgBtn::mbCancel, 0);
              if (r == mrYes)
                AResult = true;
              else if (r == mrCancel)
                AResult = false;

}

void __fastcall TEdgeMainForm::OnProtocolPropsConflictDelete(String AModuleName, String AProtocolProps, bool &AResult)
{
  int r = MessageDlg(Format("ModuleName '%0:s' has conflicting connection properties (%1:s).  Delete '%0:s?'",
        ARRAYOFCONST((AModuleName, AProtocolProps))),
        TMsgDlgType::mtConfirmation, TMsgDlgButtons() << TMsgDlgBtn::mbYes << TMsgDlgBtn::mbNo << TMsgDlgBtn::mbCancel, 0);
              if (r == mrYes)
                AResult = true;
              else if (r == mrCancel)
                AResult = false;

}


void __fastcall TEdgeMainForm::OnIdle(TObject *Sender, bool &ADone)
{
  if (EdgeServiceModule->EMSEdgeService1->Active)
    Caption = String::Format("Thingpoint: %s", ARRAYOFCONST((EdgeServiceModule->EMSEdgeService1->ModuleName)));
  else
    Caption = "Thingpoint (inactive)";
}

//---------------------------------------------------------------------------




void __fastcall TEdgeMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  EdgeServiceModule->EMSEdgeService1->Active = false;  // Unregister

}
//---------------------------------------------------------------------------



void __fastcall TEdgeMainForm::HeartRateFrameButtonConnectClick(TObject *Sender)
{
  HeartRateFrame->ButtonConnectClick(Sender);
}
//---------------------------------------------------------------------------

