//---------------------------------------------------------------------------

#include <fmx.h>
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
	#if defined(__APPLE__) && defined(__arm__)
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
	throw Exception("Test Exception");
}
//---------------------------------------------------------------------------
