// ---------------------------------------------------------------------------

#ifndef FmxMultiViewCustomPresentationH
#define FmxMultiViewCustomPresentationH
// ---------------------------------------------------------------------------
#include <System.Messaging.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <FMX.MultiView.hpp>
#include <FMX.MultiView.Presentations.hpp>
#include <FMX.MultiView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Forms.hpp>
// ---------------------------------------------------------------------------
/*

 TMultiViewAlertPresentation = class(TMultiViewPresentation)
 private
 FDetailOverlay: TShadowedOverlayLayout;
 FFrame: TPanel;
 { Messaging }
 procedure DoFormReleased(const Sender: TObject; const M: TMessage);
 protected
 function GetDisplayName: string; override;
 procedure DoOpen(const ASpeed: Single); override;
 procedure DoClose(const ASpeed: Single); override;
 procedure DoInstall; override;
 procedure DoUninstall; override;
 { Mouse events }
 procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); virtual;
 public
 constructor Create(AMultiView: TCustomMultiView); override;
 destructor Destroy; override;
 procedure UpdateSettings; override;
 procedure Realign; override;
 end;
 */

class TMultiViewAlertPresentation : public TMultiViewPresentation {
private:
	TShadowedOverlayLayout * FDetailOverlay;
	TPanel * FFrame;

	/* Messaging */
	void __fastcall DoFormReleased(System::TObject* const Sender, System::Messaging::TMessage* const M);

protected:
	System::UnicodeString __fastcall GetDisplayName(void);
	void __fastcall DoInstall(void);
	void __fastcall DoUninstall(void);
	void __fastcall DoOpen(const float ASpeed = 0.000000E+00f);
	void __fastcall DoClose(const float ASpeed = 0.000000E+00f);
	/* Mouse events */
	void virtual __fastcall DoMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  float X, float Y);
public:
	__fastcall TMultiViewAlertPresentation(TCustomMultiView* AMultiView);
	__fastcall ~TMultiViewAlertPresentation(void);
	void __fastcall Realign(void);
	void __fastcall UpdateSettings(void);
};
// ---------------------------------------------------------------------------

#endif
