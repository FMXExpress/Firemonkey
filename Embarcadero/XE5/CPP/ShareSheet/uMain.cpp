//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
