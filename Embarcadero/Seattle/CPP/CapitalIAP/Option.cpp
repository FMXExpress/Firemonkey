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

