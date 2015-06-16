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
