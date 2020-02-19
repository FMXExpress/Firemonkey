//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
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
class TForm3D4 : public TForm3D
{
__published:	// IDE-managed Components
	TSphere *Sphere1;
	TColorMaterialSource *ColorMaterialSource1;
	TCube *Cube1;
	TColorMaterialSource *ColorMaterialSource2;
	TCylinder *Cylinder1;
	TColorMaterialSource *ColorMaterialSource3;
	TRectangle3D *Rectangle3D1;
	TCamera *Camera1;
	TFloatAnimation *FloatAnimation1;
	void __fastcall Rectangle3D1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm3D4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D4 *Form3D4;
//---------------------------------------------------------------------------
#endif
