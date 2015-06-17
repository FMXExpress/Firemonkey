//---------------------------------------------------------------------------

#ifndef TapHoldFormH
#define TapHoldFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TTapHold : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TToolBar *ToolBar1;
	TLabel *Title;
	TGestureManager *GestureManager1;
	void __fastcall FormGesture(TObject *Sender, const TGestureEventInfo &EventInfo,
          bool &Handled);
private:	// User declarations
public:		// User declarations
	__fastcall TTapHold(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTapHold *TapHold;
//---------------------------------------------------------------------------
#endif
