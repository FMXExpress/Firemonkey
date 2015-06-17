
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

TOrientationSensorForm *OrientationSensorForm;

// ---------------------------------------------------------------------------
__fastcall TOrientationSensorForm::TOrientationSensorForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TOrientationSensorForm::FormActivate(TObject *Sender)
{
//#if defined(TARGET_IPHONE_SIMULATOR)
//	lbOrientationSensor->Text = "Simulator - no sensors";
//	swOrientationSensorActive->Enabled = false;
//#endif
}
// ---------------------------------------------------------------------------

void __fastcall TOrientationSensorForm::HeadingButtonClick(TObject *Sender) {
	OrientationSensor1->Active = false;
	TiltButton->IsPressed = false;
	HeadingButton->IsPressed = true;
	OrientationSensor1->Active = swOrientationSensorActive->IsChecked;
}

// ---------------------------------------------------------------------------
void __fastcall TOrientationSensorForm::TiltButtonClick(TObject *Sender) {
	OrientationSensor1->Active = false;
	HeadingButton->IsPressed = false;
	TiltButton->IsPressed = true;
	OrientationSensor1->Active = swOrientationSensorActive->IsChecked;
}

// ---------------------------------------------------------------------------
void __fastcall TOrientationSensorForm::OrientationSensor1SensorChoosing(TObject *Sender,
		  const TSensorArray Sensors, int &ChoseSensorIndex)
{
	int Found = -1;
	for(int i = 0; i < Sensors.Length; i++) {
		if ((TiltButton->IsPressed) &&
			(static_cast<TCustomOrientationSensor*>(Sensors[i])->
				AvailableProperties.Contains(TCustomOrientationSensor::TProperty::TiltX))) {
			Found = i;
			break;
		}
		else if ((HeadingButton->IsPressed) && (static_cast<TCustomOrientationSensor*>(Sensors[i])->
			AvailableProperties.Contains(TCustomOrientationSensor::TProperty::HeadingX))) {
			Found = i;
			break;
		}
	}
	if ( Found < 0) {
		Found = 0;
		TiltButton->IsPressed = true;
		HeadingButton->IsPressed = false;
		ShowMessage("Compass not available");
	}
	ChoseSensorIndex = Found;
}

//---------------------------------------------------------------------------

void __fastcall TOrientationSensorForm::swOrientationSensorActiveSwitch(TObject *Sender)

{
	// activate or deactivate the orientation sensor
	OrientationSensor1->Active = swOrientationSensorActive->IsChecked;
	Timer1->Enabled = swOrientationSensorActive->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TOrientationSensorForm::Timer1Timer(TObject *Sender)
{
	// get the data from the sensor
	lbTiltX->Text = lbTiltX->Text.sprintf(L"Tilt X: %f",
		OrientationSensor1->Sensor->TiltX);
	lbTiltY->Text = lbTiltY->Text.sprintf(L"Tilt Y: %f",
		OrientationSensor1->Sensor->TiltY);
	lbTiltZ->Text = lbTiltZ->Text.sprintf(L"Tilt Z: %f",
		OrientationSensor1->Sensor->TiltZ);
	lbHeadingX->Text = lbHeadingX->Text.sprintf(L"Heading X: %f",
		OrientationSensor1->Sensor->HeadingX);
	lbHeadingY->Text = lbHeadingY->Text.sprintf(L"Heading Y: %f",
		OrientationSensor1->Sensor->HeadingY);
	lbHeadingZ->Text = lbHeadingZ->Text.sprintf(L"Heading Z: %f",
		OrientationSensor1->Sensor->HeadingZ);
}
//---------------------------------------------------------------------------

