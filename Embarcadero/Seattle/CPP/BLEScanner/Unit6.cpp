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

#include "Unit6.h"
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
void __fastcall TForm6::Button1Click(TObject *Sender)
{
	if(!Scanning) {
		ListBox1->Clear();
		TimeScanning = 0;
		BluetoothLE1->DiscoverDevices(ScanningTime);
		ProgressBar1->Value = 0;
		Timer1->Enabled = true;
		Scanning = true;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm6::Button2Click(TObject *Sender)
{
	BluetoothLE1->CancelDiscovery();
	Timer1->Enabled = false;
	Scanning = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::FormShow(TObject *Sender)
{
	Scanning = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::Timer1Timer(TObject *Sender)
{
	TimeScanning++;
	int partial = ScanningTime / 10000;
	ProgressBar1->Value = ((Timer1->Interval / 100) / partial) * TimeScanning;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item)
{
	Button2Click(Sender);
	ListBox2->Clear();
	ListBox2->Items->Add("- Discovering services -->");
	TThread::CreateAnonymousThread([this]{
		if(!BluetoothLE1->DiscoveredDevices->Items[ListBox1->ItemIndex]->DiscoverServices()) {
			TThread::Synchronize(NULL, [this]{
				ListBox2->Items->Add("- Discover services not allow");
				ListBox1->Enabled = true;
			});
		}
	})->Start();
	ListBox1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::BluetoothLE1DiscoverLEDevice(TObject * const Sender, TBluetoothLEDevice * const ADevice,
		  int Rssi, TScanResponse * const ScanResponse)
{
	int dCount = BluetoothLE1->DiscoveredDevices->Count;
	int numOfDevices = ListBox1->Count;
	for(int i = 0; i < dCount; i++) {
		String Name = BluetoothLE1->DiscoveredDevices->Items[i]->DeviceName;
		if(Name == "") {
			Name = "Unknown device";
		}
		Name = " - " + Name + " - " + BluetoothLE1->DiscoveredDevices->Items[i]->Identifier();
		if(numOfDevices == i) {
			ListBox1->Items->Add(String().sprintf(L"%d%s", numOfDevices+1, Name.c_str()));
		} else {
			ListBox1->Items[i].Text = String().sprintf(L"%d%s", i+1, Name.c_str());
        }
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList)
{
  Timer1->Enabled = false;
  Scanning = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1ServicesDiscovered(TObject * const Sender, TBluetoothGattServiceList * const AServiceList)
{
	if(AServiceList->Count > 0) {
		for(int i = 0; i < AServiceList->Count; i++) {
			auto service = AServiceList->Items[i];
			ListBox2->Items->Add(String().sprintf(L"%d - %s - %s", i+1, service->UUIDName.c_str(),
				GUIDToString(service->UUID).c_str()));
			for(int c = 0; service->Characteristics->Count; c++) {
				auto chara = service->Characteristics->Items[c];
				ListBox2->Items->Add(String().sprintf(L"    - %s - %s", chara->UUIDName.c_str(),
					GUIDToString(chara->UUID).c_str()));
            }
        }
	}
	else {
		ListBox2->Items->Add("- Not allow access or no services");
	}
	ListBox1->Enabled = true;
}
//---------------------------------------------------------------------------

