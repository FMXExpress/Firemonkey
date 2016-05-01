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

#include "Unit3.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm3 *Form3;
//---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Beacon1BeaconEnter(TObject * const Sender, IBeacon * const ABeacon, const TBeaconList CurrentBeaconList)

{
  Memo1->Lines->Add("New Beacon");
  Memo1->Lines->Add( "UUID: " + GUIDToString(ABeacon->GUID) + " Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor));
  Memo1->Lines->Add("Current Beacons count :" + static_cast<int>(CurrentBeaconList.Length));
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Beacon1BeaconExit(TObject * const Sender, IBeacon * const ABeacon, const TBeaconList CurrentBeaconList)

{
  Memo1->Lines->Add("Beacon exited");
  Memo1->Lines->Add( "UUID: " + GUIDToString(ABeacon->GUID) + " Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor));
  Memo1->Lines->Add("Current Beacons count :" + static_cast<int>(CurrentBeaconList.Length));
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Button1Click(TObject *Sender)
{
  Beacon1->Enabled = True;
}
//---------------------------------------------------------------------------
