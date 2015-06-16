//---------------------------------------------------------------------------

#ifndef aboutboxfrmH
#define aboutboxfrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Viewport3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Materials.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmAbout : public TForm
{
__published:	// IDE-managed Components
	TRectangle *Rectangle1;
	TShadowEffect *ShadowEffect1;
	TButton *Button1;
	TText *Text1;
	TViewport3D *Viewport3D1;
	TLight *Light1;
	TCube *Cube1;
	TFloatAnimation *FloatAnimation1;
	TColorAnimation *ColorAnimation1;
	TText3D *Text3D1;
	TGlowEffect *GlowEffect1;
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmAbout(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAbout *frmAbout;
//---------------------------------------------------------------------------
#endif
