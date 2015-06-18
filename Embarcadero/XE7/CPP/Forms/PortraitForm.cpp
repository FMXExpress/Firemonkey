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

#include "PortraitForm.h"
#include "LandscapeForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TPForm *PForm;
//---------------------------------------------------------------------------
__fastcall TPForm::TPForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPForm::FormResize(TObject *Sender)
{
	if ((Height < Width)  && (Visible) &&  (LSForm != NULL)) {
		LSForm->Show();
	}
}
//---------------------------------------------------------------------------

void __fastcall TPForm::FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift)
{
#ifdef ANDROID
  if (Key == vkHardwareBack)
	Key = 0; // avoid the default back action for "first" form.
#endif

}
//---------------------------------------------------------------------------

