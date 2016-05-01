
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.UITypes.hpp>
#pragma hdrstop

#include "TapHoldForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TTapHold *TapHold;
//---------------------------------------------------------------------------
__fastcall TTapHold::TTapHold(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTapHold::FormGesture(TObject *Sender, const TGestureEventInfo &EventInfo,
          bool &Handled)
{
	// if a long tap gesture is detected
	if (EventInfo.GestureID == static_cast<short>(igiLongTap)) {
		// Show a message
		Title->Text = Title->Text.sprintf(L"LongTap at %d, %d",
			static_cast<int>(EventInfo.Location.X), static_cast<int>(EventInfo.Location.Y));
	}
}
//---------------------------------------------------------------------------

