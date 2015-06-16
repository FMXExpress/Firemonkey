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
