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
  Memo1->Lines->Add("Current Beacons count :" + IntToStr(CurrentBeaconList.Length));
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Beacon1BeaconExit(TObject * const Sender, IBeacon * const ABeacon, const TBeaconList CurrentBeaconList)

{
  Memo1->Lines->Add("Beacon exited");
  Memo1->Lines->Add( "UUID: " + GUIDToString(ABeacon->GUID) + " Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor));
  Memo1->Lines->Add("Current Beacons count :" + IntToStr(CurrentBeaconList.Length));
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Button1Click(TObject *Sender)
{
  Beacon1->Enabled = True;
}
//---------------------------------------------------------------------------
