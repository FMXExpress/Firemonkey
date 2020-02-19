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

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

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
