//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

void __fastcall TForm1::SpeedButton1Click(TObject *Sender) {
	MapView1->MapType = TMapType::Normal;
	TrackBar1->Value = 0.0;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::SpeedButton2Click(TObject *Sender) {
	MapView1->MapType = TMapType::Satellite;
	TrackBar1->Value = 0.0;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::SpeedButton3Click(TObject *Sender) {
	MapView1->MapType = TMapType::Hybrid;
	TrackBar1->Value = 0.0;
}
// ---------------------------------------------------------------------------



void __fastcall TForm1::Button1Click(TObject *Sender) {
	TMapCoordinate mapCenter = TMapCoordinate::Create(StrToFloat(edLat->Text, TFormatSettings::Invariant()),
		StrToFloat(edLong->Text, TFormatSettings::Invariant()));
	MapView1->Location = mapCenter;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::MapView1MapClick(const TMapCoordinate &Position) {
	TMapMarkerDescriptor myMarker = TMapMarkerDescriptor::Create(Position, "MyMarker");
	myMarker.Draggable = true;
	myMarker.Visible = true;
	MapView1->AddMarker(myMarker);
}
// ---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
   MapView1->Bearing = TrackBar1->Value;
}
//---------------------------------------------------------------------------

