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
void __fastcall TForm3D4::Button1Click(TObject *Sender)
{
  Button1->Text = "42!";
}
//---------------------------------------------------------------------------
void __fastcall TForm3D4::CheckBox1Change(TObject *Sender)
{
  FloatAnimation1->Enabled = CheckBox1->IsChecked;
  FloatAnimation2->Enabled = CheckBox1->IsChecked;
  FloatAnimation3->Enabled = CheckBox1->IsChecked;
}
//---------------------------------------------------------------------------
