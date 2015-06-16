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
//---------------------------------------------------------------------------
class TPinchZoom : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TGestureManager *GestureManager1;
	void __fastcall FormGesture(TObject *Sender, const TGestureEventInfo &EventInfo, bool &Handled);
private:	// User declarations
	int FLastDistance = 0;
public:		// User declarations
	__fastcall TPinchZoom(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPinchZoom *PinchZoom;
//---------------------------------------------------------------------------
#endif
