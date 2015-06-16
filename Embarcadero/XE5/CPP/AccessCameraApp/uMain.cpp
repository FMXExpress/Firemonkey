// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TAccessCameraAppForm *AccessCameraAppForm;

// ---------------------------------------------------------------------------
__fastcall TAccessCameraAppForm::TAccessCameraAppForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TAccessCameraAppForm::TakePhotoFromCameraAction1DidFinishTaking
	(TBitmap *Image)
{
	/* Assign the image retrieved from the Camera to the TImage component. */
	imgCameraImage->Bitmap->Assign(Image);
}
// ---------------------------------------------------------------------------
