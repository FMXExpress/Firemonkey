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

#include "LandscapeForm.h"
#include "PortraitForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TLSForm *LSForm;
//---------------------------------------------------------------------------
__fastcall TLSForm::TLSForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TLSForm::FormCreate(TObject *Sender)
{
	FCreated = True;
}
//---------------------------------------------------------------------------

void __fastcall TLSForm::FormResize(TObject *Sender)
{
	if((Height > Width) && (Visible) && (PForm != NULL)) {
        PForm->Show();
    }
}
//---------------------------------------------------------------------------

