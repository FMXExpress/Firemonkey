// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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

