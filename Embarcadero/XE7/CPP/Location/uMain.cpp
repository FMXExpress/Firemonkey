
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
#include <stdlib.h>
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm2 *Form2;

// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm2::swLocationSensorActiveSwitch(TObject *Sender) {
	// activate or deactivate the location sensor
	LocationSensor1->Active = swLocationSensorActive->IsChecked;
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

void __fastcall TForm2::LocationSensor1LocationChanged(TObject *Sender,
	const TLocationCoord2D &OldLocation, const TLocationCoord2D &NewLocation)
{
	String LGoogleMapsURL = "https://maps.google.com/maps?q=%s,%s";

	// convert the location to latitude and longitude

	lbLatitude->Text = String().sprintf(L"%2.5f", NewLocation.Latitude);
	lbLongitude->Text = String().sprintf(L"%2.5f", NewLocation.Longitude);
	TVarRec vr[] = {lbLatitude->Text, lbLongitude->Text};

	// and track the location via Google Maps

	WebBrowser1->Navigate(Format(LGoogleMapsURL, vr, 2));
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::nbAccuracyChange(TObject *Sender) {
	// set the triggering distance
	LocationSensor1->Accuracy = nbAccuracy->Value;
}
// ---------------------------------------------------------------------------
void __fastcall TForm2::nbTriggerDistanceChange(TObject *Sender)
{
	// set the triggering distance
	LocationSensor1->Distance = nbTriggerDistance->Value;
}
//---------------------------------------------------------------------------


