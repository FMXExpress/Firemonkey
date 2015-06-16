//---------------------------------------------------------------------------

#ifndef MainFrm_TabletH
#define MainFrm_TabletH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "MainFrm.h"
#include <FMX.ActnList.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.Filter.Effects.hpp>
//---------------------------------------------------------------------------
class TTabletMainForm : public TBaseMainForm
{
__published:	// IDE-managed Components
	TLayout *TopHelp;
	TImage *Image1;
	TText *Text1;
	TImage *Image2;
	TText *Text2;
	TSpeedButton *SpeedButton11;
	TSpeedButton *SpeedButton7;
	TSpeedButton *SpeedButton8;
	TAction *ActionWaveEffect;
	TAction *ActionContrastEffect;
	TAction *ActionPaperSketchEffect;
	void __fastcall ActionWaveEffectExecute(TObject *Sender);
	void __fastcall ActionContrastEffectExecute(TObject *Sender);
	void __fastcall ActionPaperSketchEffectExecute(TObject *Sender);
	void __fastcall ActionListUpdate(TBasicAction *Action, bool &Handled);
private:	// User declarations
public:		// User declarations
	__fastcall TTabletMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTabletMainForm *TabletMainForm;
//---------------------------------------------------------------------------
#endif
