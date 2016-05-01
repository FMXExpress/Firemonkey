
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
TForm2 *Form2;

// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm2::btnBackClick(TObject *Sender) {
	/* move back one page in the history */
	WebBrowser1->GoBack();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::btnForwardClick(TObject *Sender) {
	/* move forward one page in the history */
	WebBrowser1->GoForward();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::btnGOClick(TObject *Sender) {
	/* Passing the URL entered in the edit-box to the Web Browser component. */
	WebBrowser1->URL = edtURL->Text;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::edtURLKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift)
{
  if (Key == vkReturn){
	/* navigate and hide the virtual keyboard when setting focus to GO button */
	WebBrowser1->URL = edtURL->Text;
	btnGO->SetFocus();
  }
}
//---------------------------------------------------------------------------

