//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.UITypes.hpp>
#pragma hdrstop

#include "TapHoldForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
	if (EventInfo.GestureID == igiLongTap) {
		// Show a message
		Title->Text = Title->Text.sprintf(L"LongTap at %d, %d",
			static_cast<int>(EventInfo.Location.X), static_cast<int>(EventInfo.Location.Y));
	}
}
//---------------------------------------------------------------------------

