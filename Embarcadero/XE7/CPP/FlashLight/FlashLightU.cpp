//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "FlashLightU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TFlashLightForm *FlashLightForm;
//---------------------------------------------------------------------------
__fastcall TFlashLightForm::TFlashLightForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::SetFlashlightState(bool Active)
{
	if (Active) {
		Camera->TorchMode = TTorchMode::ModeOn;
	}
	else {
		Camera->TorchMode = TTorchMode::ModeOff;
    }
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::FormCreate(TObject *Sender)
{
	ImageOff->Enabled = Camera->HasFlash;
	Camera->Active = True;
}
//---------------------------------------------------------------------------

void __fastcall TFlashLightForm::ImageOffClick(TObject *Sender)
{
	ImageOff->Visible = false;
	ImageOn->Visible = true;
	SetFlashlightState(true);
	Light->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TFlashLightForm::ImageOnClick(TObject *Sender)
{
	ImageOff->Visible = true;
	ImageOn->Visible = false;
	SetFlashlightState(false);
	Light->Visible = false;
}
//---------------------------------------------------------------------------

