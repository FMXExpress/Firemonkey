//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "LocationDemoUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::Switch1Switch(TObject *Sender)
{
	LocationSensor1->Active = Switch1->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::OnGeocodeReverseEvent(TCivicAddress* const Address)
{
	if (Address != NULL){
		ListBoxItemAdminArea->ItemData->Detail       = Address->AdminArea;
		ListBoxItemCountryCode->ItemData->Detail     = Address->CountryCode;
		ListBoxItemCountryName->ItemData->Detail     = Address->CountryName;
		ListBoxItemFeatureName->ItemData->Detail     = Address->FeatureName;
		ListBoxItemLocality->ItemData->Detail        = Address->Locality;
		ListBoxItemPostalCode->ItemData->Detail      = Address->PostalCode;
		ListBoxItemSubAdminArea->ItemData->Detail    = Address->SubAdminArea;
		ListBoxItemSubLocality->ItemData->Detail     = Address->SubLocality;
		ListBoxItemSubThoroughfare->ItemData->Detail = Address->SubThoroughfare;
		ListBoxItemThoroughfare->ItemData->Detail    = Address->Thoroughfare;
	}
}
//---------------------------------------------------------------------------


void __fastcall TForm2::LocationSensor1LocationChanged(TObject *Sender, const TLocationCoord2D &OldLocation,
          const TLocationCoord2D &NewLocation)
{
	WideChar LDecSeparator = FormatSettings.DecimalSeparator;
	TFormatSettings LSettings = FormatSettings;
	FormatSettings.DecimalSeparator = '.';
	// Show current location
	ListBoxItemLatitude->ItemData->Detail =
		ListBoxItemLatitude->ItemData->Detail.sprintf(L"%2.6f", NewLocation.Latitude);
	ListBoxItemLongitude->ItemData->Detail =
		ListBoxItemLongitude->ItemData->Detail.sprintf(L"%2.6f", NewLocation.Longitude);
	// Show Map using Google Maps
	auto LLongitude = FloatToStr(NewLocation.Longitude, LSettings);
	String URLString = "";
	URLString = URLString.sprintf(L"https://maps.google.com/maps?q=%2.6f,%2.6f&output=embed",
		NewLocation.Latitude, NewLocation.Longitude);

	FormatSettings.DecimalSeparator = LDecSeparator;
	WebBrowser1->Navigate(URLString);

	// Setup an instance of TGeocoder
	if (FGeocoder == NULL) {
		if (TGeocoder::Current != NULL) {
			FGeocoder = (TGeocoder*)new TGeocoderClass(TGeocoder::Current);
		}
		if (FGeocoder != NULL) {
			FGeocoder->OnGeocodeReverse = OnGeocodeReverseEvent;
		}
	}
	// Translate location to address
	if ((FGeocoder != NULL) && (FGeocoder->Geocoding)) {
		FGeocoder->GeocodeReverse(NewLocation);
	}
}
//---------------------------------------------------------------------------

