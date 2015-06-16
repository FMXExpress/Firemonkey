
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm3D2 *Form3D2;
//---------------------------------------------------------------------------
__fastcall TForm3D2::TForm3D2(TComponent* Owner)
	: TForm3D(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm3D2::Form3DCreate(TObject *Sender)
{
	// attempt to get and activate the sensor manager
	FSensorManager = TSensorManager::Current;
	FSensorManager->Activate();
	// attempt to get an orientation sensor
	FSensors = TSensorManager::Current->GetSensorsByCategory(TSensorCategory::Orientation);

	if (FSensors.Length == 0) {
		Label1->Text = "Gyro not found";
		return; // no sensors available
	}

	if (!(static_cast<TCustomOrientationSensor*>(FSensors[0])->Started)) {
		static_cast<TCustomOrientationSensor*>(FSensors[0])->Start();
		Timer1->Enabled = true;
	}
}
//------------------------------------------------------------------------------
void __fastcall TForm3D2::Timer1Timer(TObject *Sender)
{
	if (FSensors.Length > 0) {
		if (FSensors[0]) {
			TCustomOrientationSensor * sensor = static_cast<TCustomOrientationSensor*>(FSensors[0]);
			Rectangle3D1->RotationAngle->X = sensor->TiltX;
			Rectangle3D1->RotationAngle->Y = sensor->TiltY;
			Rectangle3D1->RotationAngle->Z = sensor->TiltZ;
				}
		Label1->Text = Label1->Text.sprintf(L"Gyro: %3.1f %3.1f %3.1f",
			Rectangle3D1->RotationAngle->X, Rectangle3D1->RotationAngle->Y,
				Rectangle3D1->RotationAngle->Z);
	}
}
//---------------------------------------------------------------------------

