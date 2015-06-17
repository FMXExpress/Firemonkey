
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
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

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

	FSensor = NULL ;
	for (int i = 0; i < FSensors.Length ; i++) {
	   if (static_cast<TCustomOrientationSensor*>(FSensors[i])->SensorType == TOrientationSensorType::Inclinometer3D) {
		 FSensor = static_cast<TCustomOrientationSensor*>(FSensors[i]);
		 break;
	   }
	 }

	if (FSensor==NULL) {
		Label1->Text = "Gyro not found";
		return; // no sensors available
	}

	//start the sensor if it is not started
	if (!(FSensor->Started)) {
		FSensor->Start();
		Timer1->Enabled = true;
	}
}
//------------------------------------------------------------------------------
void __fastcall TForm3D2::Timer1Timer(TObject *Sender)
{
	if (FSensors.Length > 0) {
		if (!(FSensor)==NULL) {
		 #if defined(__ANDROID__) //In Android, Tilt property is returned as vector
			Rectangle3D1->RotationAngle->X = FSensor->TiltX * 360;
			Rectangle3D1->RotationAngle->Y = FSensor->TiltY * 360;
			Rectangle3D1->RotationAngle->Z = FSensor->TiltZ * 360;
		 #else //In other platforms, Tilt property is returned as degree
			Rectangle3D1->RotationAngle->X = FSensor->TiltX;
			Rectangle3D1->RotationAngle->Y = FSensor->TiltY;
			Rectangle3D1->RotationAngle->Z = FSensor->TiltZ;
		 #endif
				}
		Label1->Text = Label1->Text.sprintf(L"Gyro: %3.1f %3.1f %3.1f",
			Rectangle3D1->RotationAngle->X, Rectangle3D1->RotationAngle->Y,
				Rectangle3D1->RotationAngle->Z);
	}
}
//---------------------------------------------------------------------------

