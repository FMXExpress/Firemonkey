//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uGlassLevelH
#define uGlassLevelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Sensors.Components.hpp>
#include <System.Sensors.hpp>
#include <FMX.Controls.Presentation.hpp>
#if defined(__ANDROID__) && defined(__arm__)
	#include "Androidapi.JNI.PowerManager.hpp"
#endif
#include <vector>
#include <algorithm>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TLabel *LabelX;
	TLabel *LabelZ;
	TLine *LineActive;
	TFloatAnimation *FloatAnimationX;
	TFloatAnimation *FloatAnimationZ;
	TLine *LineReference;
	TMotionSensor *MotionSensor1;
	TStyleBook *StyleBook1;
	TTimer *Timer1;
	void __fastcall MotionSensor1DataChanged(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FloatAnimationXFinish(TObject *Sender);
	void __fastcall FloatAnimationZFinish(TObject *Sender);
private:	// User declarations
	std::vector<float> FXSample;
	std::vector<float> FZSample;
	void __fastcall UpdateZ();
	void __fastcall UpdateX();

	bool __fastcall HandleAppEvent(TApplicationEvent AAppEvent, System::TObject* AContext);
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
const int SampleCount = 20;
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
