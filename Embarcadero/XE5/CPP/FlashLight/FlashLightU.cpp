//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "FlashLightU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
		Camera->TorchMode = TTorchMode::tmModeOn;
	}
	else {
		Camera->TorchMode = TTorchMode::tmModeOff;
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

