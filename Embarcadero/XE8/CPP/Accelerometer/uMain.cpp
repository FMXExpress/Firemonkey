
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TAccelerometerForm *AccelerometerForm;

// ---------------------------------------------------------------------------
__fastcall TAccelerometerForm::TAccelerometerForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TAccelerometerForm::FormCreate(TObject *Sender) {
	// start with no sensors
	lbAccelerationX->Visible = false;
	lbAccelerationY->Visible = false;
	lbAccelerationZ->Visible = false;
	lbAngleAccelX->Visible = false;
	lbAngleAccelY->Visible = false;
	lbAngleAccelZ->Visible = false;
	lbMotion->Visible = false;
	lbSpeed->Visible = false;

#ifdef _WIN32
	lbAccelerometerSensor->Text = "Simulator - no sensors";
	swAccelerometerSensorActive->Enabled = False;
#endif
}

// ---------------------------------------------------------------------------
void __fastcall TAccelerometerForm::swAccelerometerSensorActiveSwitch
	(TObject *Sender)
{
	// activate or deactivate the reading of the accelerometer sensor
	MotionSensor1->Active = swAccelerometerSensorActive->IsChecked;
	Timer1->Enabled = swAccelerometerSensorActive->IsChecked;
}

// ---------------------------------------------------------------------------
void __fastcall TAccelerometerForm::Timer1Timer(TObject *Sender) {
	// get data from the sensor
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AccelerationX)) {
		lbAccelerationX->Visible = true;
		lbAccelerationX->Text = lbAccelerationX->Text.sprintf(L"Acceleration X: %6.2f",
			MotionSensor1->Sensor->AccelerationX);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AccelerationY)) {
		lbAccelerationY->Visible = True;
		lbAccelerationY->Text = lbAccelerationY->Text.sprintf(L"Acceleration Y: %6.2f",
			MotionSensor1->Sensor->AccelerationY);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AccelerationZ)) {
		lbAccelerationZ->Visible = True;
		lbAccelerationZ->Text = lbAccelerationZ->Text.sprintf(L"Acceleration Z: %6.2f",
			MotionSensor1->Sensor->AccelerationZ);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AngleAccelX)) {
		lbAngleAccelX->Visible = True;
		lbAngleAccelX->Text = lbAngleAccelX->Text.sprintf(L"Angle X: %6.2f",
			MotionSensor1->Sensor->AngleAccelX);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AngleAccelY)) {
		lbAngleAccelY->Visible = True;
		lbAngleAccelY->Text = lbAngleAccelY->Text.sprintf(L"Angle Y: %6.2f",
			MotionSensor1->Sensor->AngleAccelY);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::AngleAccelZ)) {
		lbAngleAccelZ->Visible = True;
		lbAngleAccelZ->Text = lbAngleAccelZ->Text.sprintf(L"Angle Z: %6.2f",
			MotionSensor1->Sensor->AngleAccelZ);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::Motion)) {
		lbMotion->Visible = True;
		lbMotion->Text = lbMotion->Text.sprintf(L"Motion: %6.2f",
			MotionSensor1->Sensor->Motion);
	}
	if (MotionSensor1->Sensor->AvailableProperties.Contains
		(TCustomMotionSensor::TProperty::Speed)) {
		lbSpeed->Visible = True;
		lbSpeed->Text = lbSpeed->Text.sprintf(L"Speed: %6.2f",
			MotionSensor1->Sensor->Speed);
	}
}
// ---------------------------------------------------------------------------
