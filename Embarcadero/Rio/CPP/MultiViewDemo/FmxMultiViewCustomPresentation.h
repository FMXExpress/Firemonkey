//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef FmxMultiViewCustomPresentationH
#define FmxMultiViewCustomPresentationH
// ---------------------------------------------------------------------------
#include <System.Messaging.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <FMX.MultiView.hpp>

#include <FMX.MultiView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Forms.hpp>
// ---------------------------------------------------------------------------
class TMultiViewAlertPresentation : public TMultiViewPresentation {
private:
	TTouchInterceptingLayout* FDetailOverlay;
	TPanel* FFrame;

	/* Messaging */
	void __fastcall DoFormReleased(System::TObject* const Sender, System::Messaging::TMessageBase* const M);
protected:
	System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall DoInstall(void);
	void __fastcall DoUninstall(void);
	void __fastcall DoOpen(const float ASpeed = 0.000000E+00f);
	void __fastcall DoClose(const float ASpeed = 0.000000E+00f);
	/* Mouse events */
	void virtual __fastcall DoMouseDown(TObject* Sender, TMouseButton Button, TShiftState Shift, float X, float Y);
public:
	__fastcall TMultiViewAlertPresentation(TCustomMultiView* AMultiView);
	__fastcall ~TMultiViewAlertPresentation(void);
	void __fastcall Realign(void);
	void __fastcall UpdateSettings(void);
};
// ---------------------------------------------------------------------------

#endif
