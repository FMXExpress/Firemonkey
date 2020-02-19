//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Materials.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TForm3D1 : public TForm3D
{
__published:	// IDE-managed Components
	TLight *Light1;
	TCamera *Camera1;
	TSphere *Sphere1;
	TColorMaterialSource *ColorMaterialSource_Skyblue;
	TTextureMaterialSource *TextureMaterialSource1;
	TSphere *Sphere2;
	TTextureMaterialSource *TextureMaterialSource2;
	TFloatAnimation *FloatAnimation1;
	TFloatAnimation *FloatAnimation2;
private:	// User declarations
public:		// User declarations
	__fastcall TForm3D1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D1 *Form3D1;
//---------------------------------------------------------------------------
#endif
