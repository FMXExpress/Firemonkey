// ---------------------------------------------------------------------------

#include <fmx.h>
#include <cmath>
#pragma hdrstop

#include "ImageRotationU.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TImageRotationForm *ImageRotationForm;

double FLastAngle;

// ---------------------------------------------------------------------------
__fastcall TImageRotationForm::TImageRotationForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TImageRotationForm::FormGesture(TObject *Sender,
	const TGestureEventInfo &EventInfo, bool &Handled) {
	if (EventInfo.GestureID == igiRotate) {
		IControl * LObj = this->ObjectAtPoint(ClientToScreen(EventInfo.Location));
		if (static_cast<TImage*>((TImage*)LObj)) {
			// rotate the image
			TImage * LImage = dynamic_cast<TImage*>(LObj->GetObject());
           if (EventInfo.Flags.Contains(TInteractiveGestureFlag::gfBegin)) {
		   		FLastAngle = LImage->RotationAngle;
			} else if (EventInfo.Angle != 0) {
				LImage->RotationAngle = FLastAngle - (EventInfo.Angle * 180) / M_PI;
			}
		}
	}
}
// ---------------------------------------------------------------------------

