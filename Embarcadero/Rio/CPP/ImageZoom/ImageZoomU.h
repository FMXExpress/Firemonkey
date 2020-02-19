//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ImageZoomUH
#define ImageZoomUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TPinchZoom : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	void __fastcall FormGesture(TObject *Sender, const TGestureEventInfo &EventInfo, bool &Handled);
private:	// User declarations
	int FLastDistance;
public:		// User declarations
	__fastcall TPinchZoom(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPinchZoom *PinchZoom;
//---------------------------------------------------------------------------
#endif
