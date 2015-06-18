//---------------------------------------------------------------------------

#ifndef FlashLightUH
#define FlashLightUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TFlashLightForm : public TForm
{
__published:	// IDE-managed Components
	TLayout *ContainerLayout;
	TImage *Light;
	TGlowEffect *GlowEffect1;
	TImage *FlashLight;
	TShadowEffect *FlashLightShadow;
	TLayout *LayoutButtons;
	TImage *ImageOff;
	TImage *ImageOn;
	TCameraComponent *Camera;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ImageOffClick(TObject *Sender);
	void __fastcall ImageOnClick(TObject *Sender);
private:	// User declarations
	/* procedure SetFlashlightState(Active : Boolean); */
	void __fastcall SetFlashlightState(bool Active);
public:		// User declarations
	__fastcall TFlashLightForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFlashLightForm *FlashLightForm;
//---------------------------------------------------------------------------
#endif
