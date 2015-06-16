// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
	String LGoogleMapsURL = "https://maps.google.com/maps?q=%2.6f,%2.6f&output=embed";
	// convert the location to latitude and longitude
	lbLatitude->Text = lbLatitude->Text.sprintf(L"Latitude: %f",
		NewLocation.Latitude);
	lbLongitude->Text = lbLongitude->Text.sprintf(L"Longitude: %f",
		NewLocation.Longitude);
	TVarRec vr[] = {NewLocation.Latitude, NewLocation.Longitude};
	System::WideChar LDecimalSeparator = FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator = '.';
	// and track the location via Google Maps
	WebBrowser1->Navigate(Format(LGoogleMapsURL, vr, 2));
	FormatSettings.DecimalSeparator = LDecimalSeparator;
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

