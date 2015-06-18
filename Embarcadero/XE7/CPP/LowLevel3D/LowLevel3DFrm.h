//---------------------------------------------------------------------------

#ifndef LowLevel3DFrmH
#define LowLevel3DFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TMyMaterial : public Fmx::Materials::TCustomMaterial
{
protected:
	void __fastcall DoApply(Fmx::Types3d::TContext3D* const Context);
	void __fastcall DoInitialize(void);
	__classmethod  System::UnicodeString __fastcall DoGetMaterialProperty(const Fmx::Types3d::TMaterial::TProperty Prop);
public:
	inline __fastcall  TMyMaterial(void) : TCustomMaterial(){}
	inline __fastcall  ~TMyMaterial(void){}

};

class TForm3D2 : public TForm3D
{
__published:	// IDE-managed Components
	TTimer *Timer1;
	TBitmapObject *InputBitmap;
	TBitmapObject *SecondBitmap;
	void __fastcall Form3DRender(TObject *Sender, TContext3D *Context);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm3D2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D2 *Form3D2;
//---------------------------------------------------------------------------
#endif
