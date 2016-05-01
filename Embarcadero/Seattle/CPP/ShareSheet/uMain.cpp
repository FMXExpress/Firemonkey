
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TfrmShareSheet *frmShareSheet;
//---------------------------------------------------------------------------
__fastcall TfrmShareSheet::TfrmShareSheet(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmShareSheet::ShowShareSheetAction1BeforeExecute(TObject *Sender)
{
	// show the share sheet
	ShowShareSheetAction1->Bitmap->Assign(imgCameraPicture->Bitmap);
}
//---------------------------------------------------------------------------
void __fastcall TfrmShareSheet::TakePhotoFromCameraAction1DidFinishTaking(TBitmap *Image)

{
	imgCameraPicture->Bitmap->Assign(Image);
}
//---------------------------------------------------------------------------
