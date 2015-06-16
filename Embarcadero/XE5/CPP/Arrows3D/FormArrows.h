//---------------------------------------------------------------------------

#ifndef FormArrowsH
#define FormArrowsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Materials.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Viewport3D.hpp>
//---------------------------------------------------------------------------
class TFormArrows3d : public TForm
{
__published:	// IDE-managed Components
	TViewport3D *Viewport3D1;
	TDummy *DummyXY;
	TCamera *CameraZ;
	TDummy *Dummy1;
	TCylinder *CylX;
	TCone *ConeX;
	TCylinder *CylY;
	TCylinder *CylZ;
	TCone *ConeY;
	TCone *ConeZ;
	TLightMaterialSource *LightMaterialSourceX;
	TLightMaterialSource *LightMaterialSourceY;
	TLightMaterialSource *LightMaterialSourceZ;
	TLight *Light1;
	void __fastcall Viewport3D1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          float X, float Y);
	void __fastcall Viewport3D1MouseMove(TObject *Sender, TShiftState Shift, float X,
          float Y);
	void __fastcall Viewport3D1MouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          bool &Handled);
private:	// User declarations
  TPointF FDown;
public:		// User declarations
	__fastcall TFormArrows3d(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormArrows3d *FormArrows3d;
//---------------------------------------------------------------------------
#endif
