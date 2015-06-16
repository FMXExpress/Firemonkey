//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm3D4 *Form3D4;
//---------------------------------------------------------------------------
__fastcall TForm3D4::TForm3D4(TComponent* Owner)
	: TForm3D(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm3D4::Rectangle3D1Click(TObject *Sender)
{
   FloatAnimation1->Enabled = !FloatAnimation1->Enabled;
}
//---------------------------------------------------------------------------
