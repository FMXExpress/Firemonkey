//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <typeinfo>
#pragma hdrstop

#include "fmMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::customButtonExecute(TObject *Sender)
{
	if (Sender != NULL && Sender->InheritsFrom(__classid(TVirtualKeyboardToolButton))) {
		ShowMessage("Pressed custom toolbutton " + static_cast<TVirtualKeyboardToolButton*>(Sender)->Title);
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	if (TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXVirtualKeyboardToolbarService)) &&
		(FService = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXVirtualKeyboardToolbarService)))) {
		swToolbar->IsChecked = FService->IsToolbarEnabled();
		swDoneButton->IsChecked = FService->IsHideKeyboardButtonVisible();
	} else {
		FService = NULL;
		swToolbar->Enabled = false;
		lbButtons->Enabled = false;
		swDoneButton->Enabled = false;
		ShowMessage("Virtual keyboard service is not supported");
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnAddClick(TObject *Sender)
{
	System::String LCaption;
	if(FService != NULL && InputQuery("Add new toolbutton", "Enter button caption", LCaption) && !LCaption.IsEmpty()) {
		lbButtons->Items->Add(LCaption);
		FService->AddButton(LCaption, &(this->customButtonExecute));
	}
}
//---------------------------------------------------------------------------


void __fastcall TForm1::swToolbarSwitch(TObject *Sender)
{
	if (FService != NULL) {
		FService->SetToolbarEnabled(swToolbar->IsChecked);
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::swDoneButtonSwitch(TObject *Sender)
{
	if (FService != NULL) {
		FService->SetHideKeyboardButtonVisibility(swDoneButton->IsChecked);
	}
}
//---------------------------------------------------------------------------


void __fastcall TForm1::btnDeleteClick(TObject *Sender)
{
	if (lbButtons->Selected != NULL && FService != NULL) {
		FService->DeleteButton(lbButtons->Selected->Index-1); // -1 because of group header
		lbButtons->Items->Delete(lbButtons->Selected->Index);
	}
}
//---------------------------------------------------------------------------

