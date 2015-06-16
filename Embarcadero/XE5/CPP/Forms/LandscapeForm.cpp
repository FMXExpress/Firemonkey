//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "LandscapeForm.h"
#include "PortraitForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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

