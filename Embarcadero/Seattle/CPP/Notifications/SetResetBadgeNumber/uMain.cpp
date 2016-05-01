
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TTSettingBadgeNumberForm *TSettingBadgeNumberForm;

// ---------------------------------------------------------------------------
__fastcall TTSettingBadgeNumberForm::TTSettingBadgeNumberForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TTSettingBadgeNumberForm::btnSetBadgeNumberClick
	(TObject *Sender) {
	// set Icon Badge Number
	if (NotificationC->Supported()) {
		NotificationC->ApplicationIconBadgeNumber = nbBadgeNumber->Value;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TTSettingBadgeNumberForm::FormCreate(TObject *Sender) {
	/* display current Icon Badge Number */
	nbBadgeNumber->Value = GetBadgeNumber();
}

// ---------------------------------------------------------------------------
Single __fastcall TTSettingBadgeNumberForm::GetBadgeNumber() {
	return NotificationC->ApplicationIconBadgeNumber;
}

void __fastcall TTSettingBadgeNumberForm::btnResetBadgeNumberClick
	(TObject *Sender)

{
	/* reset Icon Badge Number */
	if (NotificationC->Supported()) {
		NotificationC->ApplicationIconBadgeNumber = 0;
	}
	nbBadgeNumber->Value = 0;
}
// ---------------------------------------------------------------------------
void __fastcall TTSettingBadgeNumberForm::btnBadgeNumberDownClick(TObject *Sender)

{
	nbBadgeNumber->Value -= 1;
}
//---------------------------------------------------------------------------

void __fastcall TTSettingBadgeNumberForm::btnBadgeNumberUpClick(TObject *Sender)
{
	nbBadgeNumber->Value += 1;
}
//---------------------------------------------------------------------------

