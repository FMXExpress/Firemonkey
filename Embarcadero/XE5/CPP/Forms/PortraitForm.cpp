//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "PortraitForm.h"
#include "LandscapeForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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

