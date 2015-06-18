
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

#include "ImageZoomU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TPinchZoom *PinchZoom;
//---------------------------------------------------------------------------
__fastcall TPinchZoom::TPinchZoom(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPinchZoom::FormGesture(TObject *Sender, const TGestureEventInfo &EventInfo,
		  bool &Handled)
{
	if (EventInfo.GestureID == static_cast<short>(igiZoom)) {
		IControl *LObj = this->ObjectAtPoint(ClientToScreen(EventInfo.Location));
		if (static_cast<TImage*>((TImage*)LObj)) {
			if (!EventInfo.Flags.Contains(TInteractiveGestureFlag::gfBegin) &&
				!EventInfo.Flags.Contains(TInteractiveGestureFlag::gfEnd)) {
				// zoom the image
				TImage * LImage = dynamic_cast<TImage*>(LObj->GetObject());
				TPointF LImageCenter = LImage->Position->Point + PointF(LImage->Width / 2,
					LImage->Height / 2);
				LImage->Width = LImage->Width + (EventInfo.Distance - FLastDistance);
				LImage->Height = LImage->Height + (EventInfo.Distance - FLastDistance);
				LImage->Position->X = LImageCenter.X - LImage->Width / 2;
				LImage->Position->Y = LImageCenter.Y - LImage->Height / 2;
			}
			FLastDistance = EventInfo.Distance;
		}
	}
}
//---------------------------------------------------------------------------

