//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layers3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Materials.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.StdCtrls.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TForm3D4 : public TForm3D
{
__published:	// IDE-managed Components
	TLayer3D *Layer3D1;
	TButton *Button1;
	TCheckBox *CheckBox1;
	TSphere *Sphere1;
	TTextureMaterialSource *TextureMaterialSource1;
	TFloatAnimation *FloatAnimation1;
	TSphere *Sphere2;
	TTextureMaterialSource *TextureMaterialSource2;
	TFloatAnimation *FloatAnimation2;
	TCamera *Camera1;
	TCamera *Camera2;
	TFloatAnimation *FloatAnimation3;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall CheckBox1Change(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm3D4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D4 *Form3D4;
//---------------------------------------------------------------------------
#endif
