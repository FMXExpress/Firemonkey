//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
#include <fmx.h>
#include <System.SysUtils.hpp>
#pragma hdrstop

#include "Unit2.h"
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
void __fastcall TForm2::Button1Click(TObject *Sender)
{
  if (FManager == NULL)
  {
	FManager = TBluetoothLEManager::Current;
	FManager->OnDiscoverLEDevice = DiscoverLEDevice;
  }
  FManager->StartDiscovery(10000);
}
//---------------------------------------------------------------------------

void MyBinToHex(unsigned char *Buffer, char *Text, int BufSize)
{
  const char Convert[] = {"0123456789ABCDEF"};

  for(int I = 0; I < BufSize; I++)
  {
	Text[I*2] = Convert[Buffer[I] >> 4];
	Text[I*2+1] = Convert[Buffer[I] & 0x0F];
  }
}

bool __fastcall MyThreadProcedure::DecodeScanResponse(TScanResponse* const ScanResponse, TBeaconDevice &BeaconDevice)
{
  const	int GUID_LENGTH = 16;
  const int	MARK_POSITION = 9;

  char LSTBuff[GUID_LENGTH * 2];
  TByteDynArray value;
  ScanResponse->TryGetValue(TScanResponseKey::ManufacturerSpecificData, value);

  if(value.Length < 23)
    return false;

  MyBinToHex(&value[BEACON_GUID_POSITION], LSTBuff, GUID_LENGTH);

  String GUID(LSTBuff, GUID_LENGTH * 2);
  GUID = "{" + GUID + "}";
  GUID.Insert0('-',MARK_POSITION);
  GUID.Insert0('-', MARK_POSITION + 5);
  GUID.Insert0('-', MARK_POSITION + 10);
  GUID.Insert0('-', MARK_POSITION + 15);

  BeaconDevice.GUID = StringToGUID(GUID);

  Word aux;
  aux = value.Length;
  aux = value[BEACON_MAJOR_POSITION];

  BeaconDevice.Major = Word(((value[BEACON_MAJOR_POSITION] << 8) & 0xff00) | ((value[BEACON_MAJOR_POSITION + 1]) & 0x00ff));
  BeaconDevice.Minor = Word(((value[BEACON_MINOR_POSITION] << 8) & 0xff00) | ((value[BEACON_MINOR_POSITION + 1]) & 0x00ff));

  if (value[BEACON_TYPE_POSITION] == char(BEACON_ST_TYPE >> 8))
  {
	BeaconDevice.TxPower = Int8(value[value.Length - 1]);
	BeaconDevice.Alt = False;
  }
  else
  {
	BeaconDevice.TxPower = Int8(value[value.Length - 2]);
	BeaconDevice.Alt = True;
  }

   return true;
}


MyThreadProcedure::MyThreadProcedure(TBluetoothLEDevice* const _ADevice, int _Rssi, TScanResponse* const _ScanResponse) :
	ADevice(_ADevice),
	Rssi(_Rssi),
	ScanResponse(_ScanResponse)
{
}

void __fastcall MyThreadProcedure::Invoke(void)
{
	TBeaconDevice LBeaconDevice;
	if(!DecodeScanResponse(ScanResponse, LBeaconDevice))
        return;
	LBeaconDevice.Rssi = Rssi;
	LBeaconDevice.Distance = Form2->FManager->RssiToDistance(Rssi, LBeaconDevice.TxPower, 0.5);
	LBeaconDevice.ADevice = ADevice;
	int NewBeacon = 0;
	bool BeaconFound = False;
	if (Form2->BeaconDeviceList.size() > 0)
	{
		for (std::list<TBeaconDevice>::iterator BD=Form2->BeaconDeviceList.begin(); BD != Form2->BeaconDeviceList.end(); ++BD)
		{
			NewBeacon++;
			if (IsEqualGUID(BD->GUID, LBeaconDevice.GUID) && (BD->Major == LBeaconDevice.Major) && (BD->Minor == LBeaconDevice.Minor))
			{
				*BD = LBeaconDevice;
				BeaconFound = True;
				break;
			}
		 }
	}
	TVarRec v[] = { LBeaconDevice.Distance };
	if (!BeaconFound)
	{
		Form2->BeaconDeviceList.push_back(LBeaconDevice);
		Form2->ListBox1->Items->Add("-----------------------------");

		String BeaconName("Beacon Found: "+ ADevice->DeviceName);
		if (LBeaconDevice.Alt)
		  BeaconName = BeaconName + "; AltB";
		else
		  BeaconName = BeaconName + "; iB";
		Form2->ListBox1->Items->Add(BeaconName);

		BeaconName = "Device Complete name: ";
		if (ScanResponse->ContainsKey(TScanResponseKey::CompleteLocalName))
		{
			TByteDynArray value;
			ScanResponse->TryGetValue(TScanResponseKey::CompleteLocalName, value);
			BeaconName = BeaconName + (TEncoding::UTF8->GetString(value));
		}
		else
		  BeaconName = BeaconName + "No Name";

		Form2->ListBox1->Items->Add(BeaconName);
		Form2->ListBox1->Items->Add(" UUID: " + GUIDToString(LBeaconDevice.GUID));
		Form2->ListBox1->Items->Add(" Major:" + IntToStr(LBeaconDevice.Major) +
									",  Minor:" + IntToStr(LBeaconDevice.Minor) +
									",  txPower: " + IntToStr(LBeaconDevice.TxPower));
		Form2->ListBox1->Items->Add(" Rssi: " +  IntToStr(LBeaconDevice.Rssi) + Format(" Distance: %f m", v, 0));
	}
	else
	{
		String BeaconName("Beacon Found: "+ ADevice->DeviceName);
		if (LBeaconDevice.Alt)
		  BeaconName = BeaconName + "; AltB";
		else
		  BeaconName = BeaconName + "; iB";
		Form2->ListBox1->Items->Strings[(NewBeacon-1)*6+1] = (BeaconName);
		Form2->ListBox1->Items->Strings[(NewBeacon-1)*6+5] = (" Rssi: " +  IntToStr(LBeaconDevice.Rssi) + Format(" Distance: %f m", v, 0));
	}
}

void __fastcall TForm2::DiscoverLEDevice(System::TObject* const Sender, TBluetoothLEDevice* const ADevice, int Rssi,
  TScanResponse* const ScanResponse)
{
	if (ScanResponse->ContainsKey(TScanResponseKey::ManufacturerSpecificData))
	{
		_di_TThreadProcedure mtp = new MyThreadProcedure(ADevice, Rssi, ScanResponse);
		System::Classes::TThread::Synchronize(NULL, mtp);
	}
}


void __fastcall TForm2::Button2Click(TObject *Sender)
{
	if (FManager != NULL)
		FManager->CancelDiscovery();
}
//---------------------------------------------------------------------------

