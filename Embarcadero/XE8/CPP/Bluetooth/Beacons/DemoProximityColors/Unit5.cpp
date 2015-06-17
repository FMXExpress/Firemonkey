//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit5.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm5 *Form5;
//---------------------------------------------------------------------------
__fastcall TForm5::TForm5(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm5::Button1Click(TObject *Sender)
{
  if (FisScanning)
  {
	FBeaconManager->StopScan();
	Button1->Text = "START";
	FisScanning = False;
  }
  else
  {
	FBeaconManager->StartScan();
	Button1->Text = "STOP";
	FisScanning = True;
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm5::FormShow(TObject *Sender)
{
  TGUID GUID;
  if (FBeaconManager == NULL)
  {
	FBeaconManager = TBeaconManager::GetBeaconManager(TBeaconScanMode::Standard);
	FBeaconManager->OnBeaconProximity = BeaconProximity;
  };
  GUID = StringToGUID("{B9407F30-F5F8-466E-AFF9-25556B57FE6D}");

  FBeaconManager->RegisterBeacon(GUID);
  FBeaconManager->CalcMode = TBeaconCalcMode::Raw;
  FBeaconManager->ScanningTime = 109;
  FBeaconManager->ScanningSleepingTime = 15;
  FBeaconManager->StartScan();
  FisScanning = True;
}
//---------------------------------------------------------------------------
void __fastcall TForm5::BeaconProximity(System::TObject* const Sender, const _di_IBeacon ABeacon, TBeaconProximity Proximity)
{
  if (Proximity == TBeaconProximity::Inmediate)
  {
	switch (ABeacon->Major)
	{
	  case 10: Form5->Fill->Color = (TAlphaColor)TAlphaColorRec::Springgreen; break;
	  case 20: Form5->Fill->Color = (TAlphaColor)TAlphaColorRec::Slateblue; break;
	  case 30: Form5->Fill->Color = (TAlphaColor)TAlphaColorRec::Aqua; break;
	};
	Company->Text = "Estimote";
	BeaconType->Text = "iBeacon";
	MajorMinor->Text = "Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor);
  };
};
//---------------------------------------------------------------------------
