// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TCameraComponentForm *CameraComponentForm;

// ---------------------------------------------------------------------------
__fastcall TCameraComponentForm::TCameraComponentForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::btnAutoClick(TObject *Sender) {
	// turn on automatic control of Flash, if supported
	if (CameraComponent1->HasFlash)
		CameraComponent1->FlashMode = TFlashMode::fmAutoFlash;
}
// ---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnBackCameraClick(TObject *Sender) {
	// select Back Camera
	CameraComponent1->Active = false;
	CameraComponent1->Kind = TCameraKind::ckBackCamera;
	CameraComponent1->Active = true;
}

// ---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::btnFrontCameraClick(TObject *Sender) {
	// select Front Camera
	CameraComponent1->Active = false;
	CameraComponent1->Kind = TCameraKind::ckFrontCamera;
	CameraComponent1->Active = true;
}
// ---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnOnClick(TObject *Sender) {
	// turn on Flash, if supported
	if (CameraComponent1->HasFlash)
		CameraComponent1->FlashMode = TFlashMode::fmFlashOn;
}
// ---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnOffClick(TObject *Sender) {
	// turn off Flash, if supported
	if (CameraComponent1->HasFlash)
		CameraComponent1->FlashMode = TFlashMode::fmFlashOff;
}
// ---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnStartCameraClick(TObject *Sender) {
	// turn on the Camera
	CameraComponent1->Active = true;
}

// ---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::btnStopCameraClick(TObject *Sender) {
	// turn off the Camera
	CameraComponent1->Active = false;
}
// ---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::GetImage()
{
	CameraComponent1->SampleBufferToBitmap(imgCameraView->Bitmap, true);
}
// ---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::CameraComponent1SampleBufferReady(TObject *Sender,
		  const __int64 ATime)
{
	GetImage();
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::FormCreate(TObject *Sender)
{
	// by default, we start with Front Camera and Flash Off
	btnFrontCamera->IsPressed = true;
	CameraComponent1->Kind = TCameraKind::ckFrontCamera;

	btnOff->IsPressed = true;
	if (CameraComponent1->HasFlash){
		CameraComponent1->FlashMode = TFlashMode::fmFlashOff;
	}
}
//---------------------------------------------------------------------------

