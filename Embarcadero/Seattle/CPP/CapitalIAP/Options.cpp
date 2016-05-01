//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Main.h"
#include "Options.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TfrOptions *frOptions;
//---------------------------------------------------------------------------
__fastcall TfrOptions::TfrOptions(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrOptions::CheckState()
{
	sCentral->HitTest = sEast->IsChecked | sWest->IsChecked;
	sEast->HitTest = sCentral->IsChecked | sWest->IsChecked;
	sWest->HitTest = sCentral->IsChecked | sEast->IsChecked;
}

void __fastcall TfrOptions::sEastSwitch(TObject *Sender)
{
	MainForm->East = sEast->IsChecked;
	CheckState();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::sCentralSwitch(TObject *Sender)
{
	MainForm->Central = sCentral->IsChecked;
	CheckState();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::sWestSwitch(TObject *Sender)
{
	MainForm->West = sWest->IsChecked;
	CheckState();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::sEuropeSwitch(TObject *Sender)
{
	MainForm->Europe = sEurope->IsChecked;
	CheckState();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::DisableAdsListBoxItemClick(TObject *Sender)
{
	MainForm->DisableAdverts();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::EuropeListBoxItemClick(TObject *Sender)
{
	MainForm->PurchaseEurope();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::RestoreAdsListBoxItemClick(TObject *Sender)
{
	MainForm->RestorePurchase();
}
//---------------------------------------------------------------------------

void __fastcall TfrOptions::ConsumeListBoxItemClick(TObject *Sender)
{
	MainForm->ConsumeProducts();
}
//---------------------------------------------------------------------------

