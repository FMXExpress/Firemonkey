//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "SettingsProjectForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TSettingsForm *SettingsForm;
//---------------------------------------------------------------------------
__fastcall TSettingsForm::TSettingsForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSettingsForm::ListBoxItemTab1Click(TObject * Sender)
{
    ChangeTabAction2->ExecuteTarget(this);
}
