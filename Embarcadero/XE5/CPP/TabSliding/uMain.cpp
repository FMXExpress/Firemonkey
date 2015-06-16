//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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

