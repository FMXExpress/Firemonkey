//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TCameraRollForm *CameraRollForm;
//---------------------------------------------------------------------------
__fastcall TCameraRollForm::TCameraRollForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCameraRollForm::TakePhotoFromLibraryAction1DidFinishTaking(TBitmap *Image)
{
	// Assign the image retrieved from the Photo Library to the TImage component.
	imgPhotoLibraryImage->Bitmap->Assign(Image);
}
//---------------------------------------------------------------------------

