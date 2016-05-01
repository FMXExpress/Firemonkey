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
#pragma hdrstop

#include "Unit6.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm6 *Form6;

class TTPDiscoverServices : public TCppInterfacedObject<TProc>
{
  public:
	TTPDiscoverServices(TForm6 *AForm) { FForm = AForm; }
	void __fastcall Invoke();
  private:
	TForm6 *FForm;
	void __fastcall SynchronizeProc();
};

void __fastcall TTPDiscoverServices::SynchronizeProc(void)
{
	FForm->ListBox2->Items->Add("- Discover services not allow");
	FForm->ListBox1->Enabled = true;
}

void __fastcall TTPDiscoverServices::Invoke(void)
{
	if(!FForm->BluetoothLE1->DiscoveredDevices->Items[FForm->ListBox1->ItemIndex]->DiscoverServices())
		TThread::Synchronize(NULL, SynchronizeProc);
}

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
		ScanningStart = TThread::GetTickCount();
		BluetoothLE1->DiscoverDevices(ScanningTime);
		ProgressBar1->Value = 0;
		Timer1->Enabled = true;
		Scanning = true;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm6::Button2Click(TObject *Sender)
{
	Timer1->Enabled = false;
	Scanning = false;
	BluetoothLE1->CancelDiscovery();
}
//---------------------------------------------------------------------------
void __fastcall TForm6::FormShow(TObject *Sender)
{
	Scanning = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::Timer1Timer(TObject *Sender)
{
	DWORD LElapsed = TThread::GetTickCount() - ScanningStart;
	ProgressBar1->Value = ProgressBar1->Max * float(LElapsed)/ScanningTime;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item)
{
	Button2Click(Sender);
	ListBox2->Clear();
	ListBox2->Items->Add("- Discovering services -->");

	TThread::CreateAnonymousThread(new TTPDiscoverServices(this))->Start();

	ListBox1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::BluetoothLE1DiscoverLEDevice(TObject * const Sender, TBluetoothLEDevice * const ADevice,
		  int Rssi, TScanResponse * const ScanResponse)
{
	int dCount = BluetoothLE1->DiscoveredDevices->Count;
	int numOfDevices = ListBox1->Count;
	String Name;
	for(int i = 0; i < dCount; i++) {
		Name = BluetoothLE1->DiscoveredDevices->Items[i]->DeviceName;
		if(Name == "")
			Name = "Unknown device";
		Name = IntToStr(i+1) + L" - " + Name + L" - " + BluetoothLE1->DiscoveredDevices->Items[i]->Identifier();
		if(numOfDevices == i) {
			 ListBox1->Items->Add(Name);
		}
		else {
			ListBox1->Items->Strings[i] = Name;
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList)
{
	if( Scanning )
		ProgressBar1->Value = ProgressBar1->Max;
	Timer1->Enabled = false;
	Scanning = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1ServicesDiscovered(TObject * const Sender, TBluetoothGattServiceList * const AServiceList)
{
	String aux;
	if(AServiceList->Count > 0) {
		for(int i = 0; i < AServiceList->Count; i++) {
			TBluetoothGattService *service = AServiceList->Items[i];
			aux = IntToStr(i+1) + L" - " + service->UUIDName + L" - " + GUIDToString(service->UUID);
			ListBox2->Items->Add(aux);
			for(int c = 0; c < service->Characteristics->Count; c++) {
				TBluetoothGattCharacteristic *chara = service->Characteristics->Items[c];
				aux = L"    - " + chara->UUIDName + L" - " + GUIDToString(chara->UUID);
				ListBox2->Items->Add(aux);
			}
        }
	}
	else {
		ListBox2->Items->Add("- Not allow access or no services");
	}
	ListBox1->Enabled = true;
}
//---------------------------------------------------------------------------

