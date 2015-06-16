//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "FormArrows.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
