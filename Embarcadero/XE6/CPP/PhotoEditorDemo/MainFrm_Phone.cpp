//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm_Phone.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MainFrm"
#pragma resource "*.fmx"
TPhoneMainForm *PhoneMainForm;
//---------------------------------------------------------------------------
__fastcall TPhoneMainForm::TPhoneMainForm(TComponent* Owner)
	: TBaseMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPhoneMainForm::ActionListUpdate(TBasicAction *Action, bool &Handled)
{
	TBaseMainForm::ActionListUpdate(Action, Handled);
    TextToSelectImage->Visible = FRawBitmap->IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TPhoneMainForm::ImageContainerClick(TObject *Sender)
{
	if (FRawBitmap->IsEmpty()) {
		ActionTakePhotoFromLibrary->ExecuteTarget(ButtonTakePhotoFromLibrary);
	}
}
//---------------------------------------------------------------------------
