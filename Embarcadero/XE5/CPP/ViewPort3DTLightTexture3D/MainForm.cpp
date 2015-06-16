//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm4 *Form4;
//---------------------------------------------------------------------------
__fastcall TForm4::TForm4(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm4::CheckBox1Change(TObject *Sender)
{
  FloatAnimation1->Enabled = ! FloatAnimation1->Enabled;
  FloatAnimation2->Enabled = ! FloatAnimation2->Enabled;
}
//---------------------------------------------------------------------------
