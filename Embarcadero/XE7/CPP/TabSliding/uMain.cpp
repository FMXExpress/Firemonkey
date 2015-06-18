
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
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TTabSlidingForm *TabSlidingForm;
//---------------------------------------------------------------------------
__fastcall TTabSlidingForm::TTabSlidingForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTabSlidingForm::ChangeTabActionPrevUpdate(TObject *Sender)
{
	if(TabControl1->TabIndex < TabControl1->TabCount - 1)
		ChangeTabActionNext->Tab = TabControl1->Tabs[TabControl1->TabIndex + 1];
	else
		ChangeTabActionNext->Tab = NULL;

	if (TabControl1->TabIndex > 0)
		ChangeTabActionPrev->Tab = TabControl1->Tabs[TabControl1->TabIndex - 1];
	else
		ChangeTabActionPrev->Tab = NULL;
}
//---------------------------------------------------------------------------

