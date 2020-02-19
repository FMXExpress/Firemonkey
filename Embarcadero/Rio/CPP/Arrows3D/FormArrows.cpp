//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "FormArrows.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TFormArrows3d *FormArrows3d;
//---------------------------------------------------------------------------
__fastcall TFormArrows3d::TFormArrows3d(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormArrows3d::Viewport3D1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, float X, float Y)
{
   FDown = ::PointF(X, Y);
}
//---------------------------------------------------------------------------
void __fastcall TFormArrows3d::Viewport3D1MouseMove(TObject *Sender, TShiftState Shift,
		  float X, float Y)
{
   if (Shift.Contains(ssLeft)) {
	  DummyXY->RotationAngle->X = DummyXY->RotationAngle->X - ((Y - FDown.Y) * 0.3);
	  DummyXY->RotationAngle->Y = DummyXY->RotationAngle->Y + ((X - FDown.X) * 0.3);
      FDown = ::PointF(X, Y);
   }
}
//---------------------------------------------------------------------------
void __fastcall TFormArrows3d::Viewport3D1MouseWheel(TObject *Sender, TShiftState Shift,
		  int WheelDelta, bool &Handled)
{
   CameraZ->Position->Z = CameraZ->Position->Z + WheelDelta/40;
}
//---------------------------------------------------------------------------
