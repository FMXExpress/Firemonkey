//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Option.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TOptionForm *OptionForm = NULL;

//---------------------------------------------------------------------------
void CreateOptions(void)
{
	if(OptionForm == NULL) {
        OptionForm = new TOptionForm(MainForm);
    }
}
//---------------------------------------------------------------------------
void ShowOptions(void)
{
	if (OptionForm == NULL) {
		OptionForm = new TOptionForm(MainForm);
	}
	OptionForm->Show();
}
//---------------------------------------------------------------------------
__fastcall TOptionForm::TOptionForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TOptionForm::Button1Click(TObject *Sender)
{
	Close();
	MainForm->OptionsDone();
}
//---------------------------------------------------------------------------

