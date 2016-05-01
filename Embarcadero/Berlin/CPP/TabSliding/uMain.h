//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TTabSlidingForm : public TForm
{
__published:	// IDE-managed Components
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TImage *Image1;
	TTabItem *TabItem2;
	TImage *Image2;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TGestureManager *GestureManager1;
	TActionList *TabActionList;
	TChangeTabAction *ChangeTabActionPrev;
	TChangeTabAction *ChangeTabActionNext;
	void __fastcall ChangeTabActionPrevUpdate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TTabSlidingForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTabSlidingForm *TabSlidingForm;
//---------------------------------------------------------------------------
#endif
