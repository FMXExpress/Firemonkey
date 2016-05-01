//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.DateUtils.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#include <memory>
#pragma resource "*.fmx"
TForm1 *Form1;

const String sWarning = "You're using the default value for the ApplicationID property.\n"
			 "Please click F1 on ApplicationID property in Object Inspector to get more information.";
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::UpdateUI(void)
{
	if(AppAnalytics1->Enabled) {
		EdStatus->Text = "connected";
	}
	else {
		EdStatus->Text = "disconnected";
	}

	if(AppAnalytics1->UserID.Length() == 0) {
		EdUserID->TextSettings->HorzAlign = TTextAlign::Center;
		EdUserID->Text = "-";
	}
	else {
		EdUserID->TextSettings->HorzAlign = TTextAlign::Leading;
        EdUserID->Text = AppAnalytics1->UserID;
	}
}
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	#if defined(__APPLE__) && (defined(__arm__) || defined(__arm64__))
	SaveState->StoragePath = System::Ioutils::TPath::GetHomePath() + "/Library/Caches";
	#else
	SaveState->StoragePath = System::Ioutils::TPath::GetHomePath();
	#endif
	SaveState->Name = "FMXAnalytics.data";
	// Recover persistent analytics data, if found
	if(SaveState->Stream->Size > 0) {
		std::unique_ptr<TBinaryReader> Reader(new TBinaryReader(SaveState->Stream, TEncoding::Unicode, false));
		AppAnalytics1->UserID = Reader->ReadString();
		AppAnalytics1->AllowTracking = Reader->ReadBoolean();
	}
	FClearState = false;
	UpdateUI();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormSaveState(TObject *Sender)
{
	SaveState->Stream->Clear();
	if(!FClearState && AppAnalytics1->AllowTracking) {
		std::unique_ptr<TBinaryWriter> Writer(new TBinaryWriter(SaveState->Stream));
		Writer->Write(AppAnalytics1->UserID);
        Writer->Write(AppAnalytics1->AllowTracking);
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EnableButtonClick(TObject *Sender)
{
	if(AppAnalytics1->ApplicationID.Length() == 0) {
		ShowMessage(sWarning);
		return;
	}
	AppAnalytics1->Enabled = true;
    UpdateUI();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearSaveStateButtonClick(TObject *Sender)
{
	SaveState->Stream->Clear();
	FClearState = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LaunchForm2ButtonClick(TObject *Sender)
{
	Form2->Show();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RaiseExceptionButtonClick(TObject *Sender)
{
	TForm * F = 0;
    F->Show();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RefreshButtonClick(TObject *Sender)
{
	UpdateUI();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CustomEventButtonClick(TObject *Sender)
{
	AppAnalytics1->TrackEvent("Customer_Timezone", "Recorded Locale with UTC Offset",
		System::Dateutils::TTimeZone::Local->DisplayName, System::Dateutils::TTimeZone::Local->UtcOffset.Hours);
}
//---------------------------------------------------------------------------

