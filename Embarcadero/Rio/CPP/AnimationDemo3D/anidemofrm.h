//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef anidemofrmH
#define anidemofrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Layers3D.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TfrmAniDemo : public TForm3D
{
__published:	// IDE-managed Components
	TLight *Light1;
	TLayer3D *ObjectLayer1;
	TLayout *Background1;
	TPath *Path1;
	TRoundRect *RoundRect1;
	TPlane *Plane1;
	TTextLayer3D *Text4;
	TRoundCube *RoundCube1;
	TFloatAnimation *FloatAnimation3;
	TSphere *Sphere1;
	TTextLayer3D *Text1;
	TSphere *Sphere2;
	TFloatAnimation *FloatAnimation1;
	TTextLayer3D *Text2;
	TStrokeCube *StrokeCube1;
	TCube *Cube1;
	TFloatAnimation *FloatAnimation2;
	TTextLayer3D *Text3;
	TPathAnimation *PathAni;
	TText *Text5;
	TText3D *Text3D1;
	TFloatAnimation *FloatAnimation4;
	TLightMaterialSource *ColorLighting;
	TLightMaterialSource *TextureLighting;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmAniDemo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAniDemo *frmAniDemo;
//---------------------------------------------------------------------------
#endif
