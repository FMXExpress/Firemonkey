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
#include <FMX.Layers3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.StdCtrls.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TForm3D1 : public TForm3D
{
__published:	// IDE-managed Components
	TLayer3D *Layer3D1;
	TButton *Button1;
	TGlowEffect *GlowEffect1;
	TFloatAnimation *FloatAnimation1;
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm3D1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D1 *Form3D1;
//---------------------------------------------------------------------------
#endif
