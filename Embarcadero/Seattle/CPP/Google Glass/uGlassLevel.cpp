//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uGlassLevel.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.GGlass.fmx", _PLAT_ANDROID)

TForm2 *Form2;

// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner) : TForm(Owner) {
}
// ---------------------------------------------------------------------------
float Average(const std::vector<float> &v)
{
	float avg = 0;
	std::vector<float>::const_iterator it;
	for(it = v.begin(); it != v.end(); it++) avg += *it;
	avg /= v.size();
	return avg;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::UpdateZ()
{
	float z = 0;
	if(!FloatAnimationZ->Running) {
		z = Average(FZSample);
		FZSample.clear();
		LabelZ->Text = String().sprintf(L"U/D: %6.2f ", (z * 100));
		FloatAnimationZ->Enabled = false;
		FloatAnimationZ->StopValue = LineReference->Position->Y + (z * (Height));
		if(FloatAnimationZ->StopValue < 0) {
			FloatAnimationZ->StopValue = 0;
		}
		else if(FloatAnimationZ->StopValue > (Height - 5)) {
			FloatAnimationZ->StopValue = Height - 5;
		}
		FloatAnimationZ->Enabled = true;
	}
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::FloatAnimationXFinish(TObject *Sender)
{
	UpdateX();
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::FloatAnimationZFinish(TObject *Sender)
{
	UpdateZ();
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::UpdateX()
{
	float x = 0;
	if(!FloatAnimationX->Running) {
		x = Average(FXSample) * -90;
		FXSample.clear();
		FloatAnimationX->Enabled = false;
		LabelX->Text = String().sprintf(L"L/R: %6.2f ", x);
		FloatAnimationX->StopValue = x;
		FloatAnimationX->Enabled = true;
    }
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::MotionSensor1DataChanged(TObject *Sender)
{
	float x = 0.0;
	float z = 0.0;
	if(MotionSensor1->Sensor != NULL) {
		for(TCustomMotionSensor::TProperty LProp = TCustomMotionSensor::TProperty::AccelerationX;
			LProp != TCustomMotionSensor::TProperty::Speed;
			LProp = static_cast<TCustomMotionSensor::TProperty>(static_cast<int>(LProp)+1))
		{
			// Get the data from the sensor
			switch (LProp) {
				case TCustomMotionSensor::TProperty::AccelerationX:
					x = MotionSensor1->Sensor->AccelerationX;
//					break;
				case TCustomMotionSensor::TProperty::AccelerationZ:
					z = MotionSensor1->Sensor->AccelerationZ;
//					break;
			}
		}
		FXSample.push_back(x);
		FZSample.push_back(z);
	}
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::FormDestroy(TObject *Sender)
{
#if defined(__ANDROID__) && defined(__arm__)
	ReleaseWakeLock();
#endif
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormCreate(TObject *Sender)
{
	_di_IFMXApplicationEventService AppEventService;
	/*  test whether the AppEventService services are supported */
	if(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXApplicationEventService)) &&
		(AppEventService = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXApplicationEventService))))
	{
		AppEventService->SetApplicationEventHandler(HandleAppEvent);
	}
}
//---------------------------------------------------------------------------
bool __fastcall TForm2::HandleAppEvent(TApplicationEvent AAppEvent, System::TObject* AContext)
{
#if defined(__ANDROID__) && defined(__arm__)
	bool _return = true;
	switch (AAppEvent) {
		case TApplicationEvent::aeFinishedLaunching:
			AcquireWakeLock;
			break;
		case TApplicationEvent::aeBecameActive:
			AcquireWakeLock;
			break;
		case TApplicationEvent::aeWillBecomeInactive:
			AcquireWakeLock;
			break;
		case TApplicationEvent::aeEnteredBackground:
			AcquireWakeLock;
			break;
		case TApplicationEvent::aeWillBecomeForeground:
			AcquireWakeLock;
			break;
		case TApplicationEvent::aeWillTerminate:
			AcquireWakeLock;
			break;
	default:
		_return = false;
	}
	return _return;
#endif
}
//---------------------------------------------------------------------------

