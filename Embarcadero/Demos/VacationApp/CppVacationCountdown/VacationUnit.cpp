//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "VacationUnit.h"
#include "System.DateUtils.hpp"
#include "System.IniFiles.hpp"
#include "System.iOUTils.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm6 *Form6;
//---------------------------------------------------------------------------
__fastcall TForm6::TForm6(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm6::MyActionExecute(TObject *Sender)
{
	VacationTabControl->ActiveTab = WeatherTabItem;
	TListBoxItem * MyListBoxItem;

	// days to vacation
	CountdownLabel->Text =
		"Days to Vacation: "
		+ IntToStr(DaysBetween(
			Now(),
			CalendarEdit1->Date)
		  );

	if (FoundAddrLatLong) {
		// uses WeatherBug REST API
		// http://developer.weatherbug.com/docs/read/WeatherBug_Rest_XML_API
		// update RESTRequest Resource property
		// with latitude, longitude and API key
		RESTRequest1->Resource =
			"REST/Direct/GetForecast.ashx?la="
			+ FloatToStr(FoundLatitude)
			+ "&lo="
			+ FloatToStr(FoundLongitude)
			+ "&ht=t&ht=i&ht=d&api_key="
			+ APIKeyString;

		// get weather temperatures
		RESTRequest1->Execute();

		// Populate listbox with temperatures for next 7 days
		// TODO: get day and night icons for condition codes
		//   For now just display strings
		WeatherListBox->Items->Clear();
		WeatherListBox->BeginUpdate();
		ClientDataSet1->First();
		while (!ClientDataSet1->Eof) {
			MyListBoxItem = new TListBoxItem(WeatherListBox);
			MyListBoxItem->Text =
			ClientDataSet1->FieldByName("dayTitle")->AsString
			  + ": High: "
			  + ClientDataSet1->FieldByName("high")->AsString
			  + ", Low: "
			  + ClientDataSet1->FieldByName("low")->AsString
			  + " "
			  + ClientDataSet1->FieldByName("dayDesc")->AsString;
			WeatherListBox->AddObject(MyListBoxItem);
			ClientDataSet1->Next();
		}
		WeatherListBox->EndUpdate();
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm6::FormCreate(TObject *Sender)
{
	TIniFile *Ini;
	// set up for Geocode
	FGeocoder = (TGeocoder*)new TGeocoderClass(TGeocoder::Current);
	FGeocoder->OnGeocode = OnGeocodeEvent;

	VacationTabControl->ActiveTab = WeatherTabItem;
	FoundLatitude = 0.0;
	FoundLongitude = 0.0;
	FoundAddrLatLong = false;
	RefreshButton->Visible = false;
	// read WeatherBug API Key from INI file
	#if defined(TARGET_OS_IPHONE) || defined(TARGET_OS_MAC)
	Ini = new TIniFile(System::Ioutils::TPath::GetDocumentsPath() + PathDelim + "weather.ini");
	#else
	Ini = new TIniFile("c:\\temp\\weather.ini");
	#endif
	APIKeyString = Ini->ReadString("WeatherBug","APIKey","");
}
//---------------------------------------------------------------------------
void __fastcall TForm6::AddressEditChange(TObject *Sender)
{
	// Address changed - get Latitude/Longitude via GeoCoding
	RefreshButton->Visible = false;
	// use address to find Latitude and Longitude
	lAddress = new TCivicAddress;
	try {
		lAddress->Address = AddressEdit->Text;
		FGeocoder->Geocode(lAddress);
	}
	__finally {
		delete lAddress;
	};
}
//---------------------------------------------------------------------------
void __fastcall TForm6::OnGeocodeEvent(System::DynamicArray<TLocationCoord2D> Coords)
{
	if (Coords[0].Latitude!= 0 && Coords[0].Longitude != 0) {
		FoundLatitude = Coords[0].Latitude;
		FoundLongitude = Coords[0].Longitude;
		FoundAddrLatLong = true;
		LatLongLabel->Text =
		  FloatToStr(Coords[0].Latitude)
		  + ","
		  + FloatToStr(Coords[0].Longitude);
    	#if defined(TARGET_OS_IPHONE)
		FormatSettings.DecimalSeparator = '.';
		String URLString = "";
		URLString = URLString.sprintf(
			L"https://maps.google.com/maps?q=%2.6f,%2.6f",
			Coords[0].Latitude, Coords[0].Longitude);
		// FormatSettings.DecimalSeparator = LDecSeparator;
		WebBrowser1->Navigate(URLString);
		#endif
		RefreshButton->Visible = true;
	}
	else {
		FoundAddrLatLong = false;
		LatLongLabel->Text = "Address not Found!";
	}
}


