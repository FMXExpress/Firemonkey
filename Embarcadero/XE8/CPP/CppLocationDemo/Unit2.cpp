
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Button1Click(TObject *Sender)
{
	Form2->AniIndicator1->Visible = true;
	Form2->AniIndicator1->Enabled = true;

	Form2->LocationSensor1->OnLocationChanged = LocationSensor1LocationChanged;
	Form2->LocationSensor1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::LocationSensor1LocationChanged(TObject *Sender, const TLocationCoord2D &OldLocation,
          const TLocationCoord2D &NewLocation)
{
	Form2->AniIndicator1->Enabled = false;
	Form2->AniIndicator1->Visible = false;

	Form2->Edit1->Text = FloatToStr(NewLocation.Latitude);
	Form2->Edit2->Text = FloatToStr(NewLocation.Longitude);
}
//---------------------------------------------------------------------------
