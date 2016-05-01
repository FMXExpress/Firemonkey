//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "TabbedMap.h"
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
void __fastcall TForm2::FormShow(TObject *Sender)
{
	MapPiter->Location = TMapCoordinate::Create(59.965, 30.35);
	MapPiter->Zoom = 10;

	MapFrisco->Location = TMapCoordinate::Create(37.82, -122.5);
	MapFrisco->Zoom = 10;
}
//---------------------------------------------------------------------------

void __fastcall TForm2::ZoomOutClick(TObject *Sender)
{
	MapPiter->Zoom  -= 1;
	MapFrisco->Zoom -= 1;
}
//---------------------------------------------------------------------------

void __fastcall TForm2::ZoomInClick(TObject *Sender)
{
	MapPiter->Zoom += 1;;
	MapFrisco->Zoom += 1;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::CameraChanged(TObject * Sender) {
	TMapView * mapView = static_cast<TMapView*>(Sender);
	if(mapView) {
		CameraInfo->Text = String().sprintf(L"At %3.3f, %3.3f Zoom=%2.1f",
			mapView->Location.Latitude,
			mapView->Location.Longitude,
			round(mapView->Zoom));
	}
}

