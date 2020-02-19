// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015-2019 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

#include <fmx.h>
#include <FMX.DialogService.hpp>
#include <stdlib.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TForm2 *Form2;
// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent *Owner) : TForm(Owner)
{
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::swLocationSensorActiveSwitch(TObject *Sender)
{
#ifdef __ANDROID__
	DynamicArray<String> permissions;
	permissions.Length = 1;
	permissions[0] = "android.permission.ACCESS_FINE_LOCATION";

	PermissionsService()->RequestPermissions(permissions,
		[this](const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults)
		{
			if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
			{
				// activate or deactivate the location sensor
				LocationSensor1->Active = swLocationSensorActive->IsChecked;
			}
			else
			{
				this->swLocationSensorActive->IsChecked = false;

				TDialogService::ShowMessage("Location permission not granted");
			}
		});
#else
	LocationSensor1->Active = swLocationSensorActive->IsChecked;
#endif
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::Button1Click(TObject *Sender) {
	nbTriggerDistance->Value -= 1;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::Button2Click(TObject *Sender) {
	nbTriggerDistance->Value += 1;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::Button3Click(TObject *Sender) {
	nbAccuracy->Value -= 1;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::Button4Click(TObject *Sender) {
	nbAccuracy->Value += 1;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::LocationSensor1LocationChanged(TObject *Sender, const TLocationCoord2D &OldLocation, const TLocationCoord2D &NewLocation)
{
	String latitude = String().sprintf(L"%2.5f", NewLocation.Latitude);
	String longitude = String().sprintf(L"%2.5f", NewLocation.Longitude);

	// convert the location to latitude and longitude
	lbLatitude->Text = "Latitude: " + latitude;
	lbLongitude->Text = "Longitude: " + longitude;

	TVarRec params[] = { lbLatitude->Text, lbLongitude->Text };

    // and track the location via Google Maps
	WebBrowser1->Navigate(Format("https://maps.google.com/maps?q=%s,%s", params, 2));
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::nbAccuracyChange(TObject *Sender)
{
	// set the triggering distance
	LocationSensor1->Accuracy = nbAccuracy->Value;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::nbTriggerDistanceChange(TObject *Sender)
{
	// set the triggering distance
	LocationSensor1->Distance = nbTriggerDistance->Value;
}
// ---------------------------------------------------------------------------
