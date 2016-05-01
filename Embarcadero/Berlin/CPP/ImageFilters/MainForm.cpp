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
#include "FMX.Filter.hpp"
#include "FMX.Filter.Effects.hpp"
#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OpenButtonClick(TObject *Sender)
{
	if (OpenDialog1->Execute()) {
		Image1->Bitmap->LoadFromFile(OpenDialog1->FileName);
		SaveButton->Enabled = true;
		MySepiaFilter->Input = Image1->Bitmap;
		MySepiaFilter->Amount = TrackBar1->Value / 100;
		Image2->Bitmap = MySepiaFilter->Output;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	MySepiaFilter = new TFilterSepia(this);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
	MySepiaFilter->Amount = TrackBar1->Value / 100;
	Image2->Bitmap = MySepiaFilter->Output;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveButtonClick(TObject *Sender)
{
	if (SaveDialog1->Execute()) {
		Image2->Bitmap->SaveToFile(SaveDialog1->FileName);
	}
}
//---------------------------------------------------------------------------
