//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "ImageZoomU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
	if (EventInfo.GestureID == igiZoom) {
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

