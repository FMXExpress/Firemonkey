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

#include "uExplorerDevices.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TFrDeviceExplorer *FrDeviceExplorer;

//---------------------------------------------------------------------------
__fastcall TFrDeviceExplorer::TFrDeviceExplorer(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::FormCreate(TObject *Sender)
{
	CurrentService = 0;
	CurrentCharacteristic = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::tmAnimateFindDevicesTimer(TObject *Sender)
{
	PbFindDevices->Value += 10;
}
//---------------------------------------------------------------------------

void __fastcall TFrDeviceExplorer::tmAnimateFindServicesTimer(TObject *Sender)
{
	if(PbServices->Value == 99)  {
		PbServices->Value = 0;
	}
	else {
		PbServices->Value++;
    }
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::DevicesDiscoveryLEEnd(TObject* const Sender,
	TBluetoothLEDeviceList* const ADeviceList)
{
	CbDevices->Items->Clear();
	for(int i = 0; i < ADeviceList->Count; i++) {
		CbDevices->Items->Add(ADeviceList->Items[i]->DeviceName);
	}
	tmAnimateFindDevices->Enabled = true;
	PbFindDevices->Value = 100;
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::Button1Click(TObject *Sender)
{
	PbFindDevices->Max = 100;
	PbFindDevices->Value = 0;
	tmAnimateFindDevices->Enabled = True;
	FBluetoothManagerLE = TBluetoothLEManager::Current;
	FBluetoothManagerLE->OnDiscoveryEnd = DevicesDiscoveryLEEnd;
	FBluetoothManagerLE->StartDiscovery(2000);
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::EmptyCharacteristic(void)
{
	lbCurrentService->Text = "";
	EdCharacName->Text = "";
	EdCharacUID->Text = "";
	cbBroadcast->IsChecked = false;
	cbExtendedProp->IsChecked = false;
	cbNotify->IsChecked = false;
	cbIndicate->IsChecked = false;
	CbRead->IsChecked = false;
	CbWrite->IsChecked = false;
	cbWriteNoResponse->IsChecked = false;
	cbSignedWrite->IsChecked = false;
	LbCurrentValue->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::CleanDeviceInformation(void)
{
	TvCharacteristics->Clear();
	EmptyCharacteristic();
	CurrentService = 0;
	CurrentCharacteristic = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::Button2Click(TObject *Sender)
{
	if(CbDevices->Selected != NULL) {
		CleanDeviceInformation();
		EdCurrentDevice->Text = CbDevices->Selected->Text;
	}
}
//---------------------------------------------------------------------------
TBluetoothLEDevice * __fastcall TFrDeviceExplorer::GetCurrentDevice()
{
	for(int i = 0; i < FBluetoothManagerLE->LastDiscoveredDevices->Count; i++) {
		if(FBluetoothManagerLE->LastDiscoveredDevices->Items[i]->DeviceName
			== EdCurrentDevice->Text) {
			return FBluetoothManagerLE->LastDiscoveredDevices->Items[i];
			}
	}
	return NULL;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFrDeviceExplorer::ConverToHex(TByteDynArray Bytes)
{
	Integer I;
	UnicodeString ResultString;
	for(int i = 0; i < Bytes.get_length(); i++){
	  ResultString = ResultString + IntToHex(Bytes.operator [](i), 1);
	}
	return ResultString;
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::RefreshCurrentCharacteristic(void)
{
	TBluetoothLEDevice * ADevice;
	ADevice = GetCurrentDevice();
	if(ADevice != NULL) {
		TBluetoothGattService * AService = ADevice->Services->Items[CurrentService];
		TBluetoothGattCharacteristic * AChar = AService->Characteristics->Items[CurrentCharacteristic];
		lbCurrentService->Text = AService->UUIDName;
		EdCharacName->Text = AChar->UUIDName;
		EdCharacUID->Text = GUIDToString(AChar->UUID);
		cbBroadcast->IsChecked = AChar->Properties.Contains(TBluetoothProperty::Broadcast);
		cbExtendedProp->IsChecked = AChar->Properties.Contains(TBluetoothProperty::ExtendedProps);
		cbNotify->IsChecked = AChar->Properties.Contains(TBluetoothProperty::Notify);
		cbIndicate->IsChecked =  AChar->Properties.Contains(TBluetoothProperty::Indicate);
		CbRead->IsChecked =  AChar->Properties.Contains(TBluetoothProperty::Read);
		CbWrite->IsChecked =  AChar->Properties.Contains(TBluetoothProperty::Write);
		cbWriteNoResponse->IsChecked =  AChar->Properties.Contains(TBluetoothProperty::WriteNoResponse);
		cbSignedWrite->IsChecked =  AChar->Properties.Contains(TBluetoothProperty::SignedWrite);

		if ((AChar->Value.Length > 0) && (CbRead->IsChecked)) {
			LbCurrentValue->Items->Clear();
			try {
				LbCurrentValue->Items->Add("HEX: " +  ConverToHex(AChar->GetValue()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("String(UTF8): " + AChar->GetValueAsString());
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("String(NOUTF8): " +  AChar->GetValueAsString(1,false));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Int8: " +  IntToStr(AChar->GetValueAsInt8()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Int16: " +  IntToStr(AChar->GetValueAsInt16()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Int32: " +  IntToStr(AChar->GetValueAsInt32()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Int64: " +  IntToStr(AChar->GetValueAsInt64()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("UInt8: " +  IntToStr(AChar->GetValueAsUInt8()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("UInt16: " +  IntToStr(AChar->GetValueAsUInt16()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("UInt32: " +  IntToStr((int)AChar->GetValueAsUInt32()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("UInt64: " +  IntToStr((__int64)AChar->GetValueAsUInt64()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Double: " + FloatToStr(AChar->GetValueAsDouble()));
			} catch (...) {
			}
			try {
			  LbCurrentValue->Items->Add("Single: " + FloatToStr(AChar->GetValueAsSingle()));
			} catch (...) {
			}
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::DidCharacteristicRead(TObject* const Sender,
	TBluetoothGattCharacteristic* const ACharacteristic, TBluetoothGattStatus AGattStatus)
{
	if(GUIDToString(ACharacteristic->UUID) == EdCharacUID->Text) {
		RefreshCurrentCharacteristic();
	}
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::btnGetValuesClick(TObject *Sender)
{
	TBluetoothLEDevice * ADevice;
	TBluetoothGattCharacteristic * AChar;
	TBluetoothGattService * AService;
	TBluetoothGattCharacteristicList * CharList;
	if(TvCharacteristics->Count > 0) {
		ADevice = GetCurrentDevice();
		if(ADevice != NULL) {
			ADevice->OnCharacteristicRead = DidCharacteristicRead;
			for(int i = 0; i < ADevice->Services->Count; i++) {
				AChar = NULL;
				AService = ADevice->Services->Items[i];
				CharList = AService->Characteristics;
				for(int j = 0; j < CharList->Count; j++) {
					AChar = CharList->Items[j];
					if ((AChar->Properties.Contains(TBluetoothProperty::Read)) ||
						(AChar->Properties.Contains(TBluetoothProperty::Notify))) {
                        ADevice->ReadCharacteristic(AChar);
                    }
                }
            }
		}
	}
	else {
        ShowMessage(EdCurrentDevice->Text + " is not available");
    }
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::ServicesDiscovered(TObject* const Sender,
	TBluetoothGattServiceList* const AServiceList)
{
	TvCharacteristics->Clear();
	for(int i = 0; i < AServiceList->Count; i++) {
		ServiceItem = new TTreeViewItem(NULL);
		ServiceItem->Parent = TvCharacteristics;
		ServiceItem->Tag = i;
		ServiceItem->IsExpanded = true;
		if (AServiceList->Items[i]->UUIDName.IsEmpty()) {
			ServiceItem->Text = "Unnamed";
		}
		else {
			ServiceItem->Text = AServiceList->Items[i]->UUIDName;
		}

		TBluetoothGattCharacteristicList *ACharList = AServiceList->Items[i]->Characteristics;
		for(j = 0; j < ACharList->Count; j++) {
			AChar = ACharList->Items[j];
			String Options = "";
			if(AChar->Properties.Contains(TBluetoothProperty::Broadcast)) Options = Options + "Broadcast ";
			if(AChar->Properties.Contains(TBluetoothProperty::ExtendedProps)) Options = Options + "ExtendedProps ";
			if(AChar->Properties.Contains(TBluetoothProperty::Notify)) Options = Options + "Notify ";
			if(AChar->Properties.Contains(TBluetoothProperty::Indicate)) Options = Options + "Indicate ";
			if(AChar->Properties.Contains(TBluetoothProperty::Read)) Options = Options + "Read ";
			if(AChar->Properties.Contains(TBluetoothProperty::Write)) Options = Options + "Write ";
			if(AChar->Properties.Contains(TBluetoothProperty::WriteNoResponse)) Options = Options + "WriteNoResponse ";
			if(AChar->Properties.Contains(TBluetoothProperty::SignedWrite)) Options = Options + "SignedWrite ";
			Characteristic = new TTreeViewItem(NULL);
			Characteristic->Parent = ServiceItem;
			Characteristic->IsExpanded = false;
			if (AChar->UUIDName != "") {
				Characteristic->Text = AChar->UUIDName;
			}
			else {
                Characteristic->Text = "Unnamed";
            }
			Characteristic->Tag = j;
			CharProps = new TTreeViewItem(NULL);
			CharProps->Tag = -1;
			CharProps->Parent = Characteristic;
			CharProps->IsExpanded = true;
			CharProps->Text = GUIDToString(AChar->UUID);
			CharProps = new TTreeViewItem(NULL);
			CharProps->Tag = -1;
			CharProps->Parent = Characteristic;
			CharProps->IsExpanded = true;
			CharProps->Text = Options;

//			TThread::Synchronize(NULL, ServicesDiscoveredThreadMethod);
			Application->ProcessMessages();
		}
	}
	tmAnimateFindServices->Enabled = false;
	PbServices->Value = 100;
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::CornerButton3Click(TObject *Sender)
{
	TBluetoothLEDevice * ADevice;
	ADevice = NULL;
	ADevice = GetCurrentDevice();
	if(ADevice != NULL) {
		PbServices->Value = 0;
		ADevice->OnServicesDiscovered = ServicesDiscovered;
		tmAnimateFindDevices->Enabled = ADevice->DiscoverServices();
	}
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::TvCharacteristicsClick(TObject *Sender)
{
	if((TvCharacteristics->Selected != NULL) && (TvCharacteristics->Selected->Tag != (NativeInt)-1) &&
		(TvCharacteristics->Selected->ParentItem() != NULL))
	{
		CurrentService = TvCharacteristics->Selected->ParentItem()->Tag;
		CurrentCharacteristic = TvCharacteristics->Selected->Tag;
		RefreshCurrentCharacteristic();
    }
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::btnRefreshClick(TObject *Sender)
{
	TBluetoothLEDevice * ADevice = NULL;
	ADevice = GetCurrentDevice();
	if(ADevice != NULL) {
		ADevice->OnCharacteristicRead = DidCharacteristicRead;
		TBluetoothGattService *AService = ADevice->Services->Items[CurrentService];
		TBluetoothGattCharacteristic *AChar = AService->Characteristics->Items[CurrentCharacteristic];
		if(AChar->Properties.Contains(TBluetoothProperty::Read)) {
			ADevice->ReadCharacteristic(AChar);
		}
		else {
			ShowMessage(EdCurrentDevice->Text + " is not available");
        }
	}
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::btnSuscribeClick(TObject *Sender)
{
	TBluetoothLEDevice * ADevice = NULL;
	ADevice = GetCurrentDevice();
	if(ADevice != NULL) {
		TBluetoothGattCharacteristic * AChar = NULL;
		TBluetoothGattService * AService = ADevice->Services->Items[CurrentService];
		AChar = AService->Characteristics->Items[CurrentCharacteristic];
		if(AChar->Properties.Contains(TBluetoothProperty::Notify)) {
			ADevice->SetCharacteristicNotification(AChar, true);
		}
		else {
			ShowMessage("This characteristic doesn't allow notifications");
		}
	}
	else {
        ShowMessage(EdCurrentDevice->Text + " is not available");
    }
}
//---------------------------------------------------------------------------
void __fastcall TFrDeviceExplorer::btnWriteClick(TObject *Sender)
{
	TBluetoothLEDevice * ADevice = NULL;
	ADevice = GetCurrentDevice();
	if(ADevice != NULL) {
		TBluetoothGattService * AService = ADevice->Services->Items[CurrentService];
		TBluetoothGattCharacteristic * AChar = AService->Characteristics->Items[CurrentCharacteristic];
		if((AChar->Properties.Contains(TBluetoothProperty::Write)) || (AChar->Properties.Contains(TBluetoothProperty::WriteNoResponse))
			|| (AChar->Properties.Contains(TBluetoothProperty::SignedWrite)))
		{
			if(CbWriteTypes->ItemIndex ==  0) AChar->SetValueAsString(EdCharacWrite->Text);
			if(CbWriteTypes->ItemIndex ==  1) AChar->SetValueAsString(EdCharacWrite->Text, false);
			if(CbWriteTypes->ItemIndex ==  2) AChar->SetValueAsUInt8(UInt8(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  3) AChar->SetValueAsUInt16(UInt16(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  4) AChar->SetValueAsUInt32(UInt32(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  5) AChar->SetValueAsUInt64(StrToInt(EdCharacWrite->Text));
			if(CbWriteTypes->ItemIndex ==  6) AChar->SetValueAsInt8(Int8(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  7) AChar->SetValueAsInt16(Int16(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  8) AChar->SetValueAsInt32(Int32(StrToInt(EdCharacWrite->Text)));
			if(CbWriteTypes->ItemIndex ==  9) AChar->SetValueAsInt64(StrToInt(EdCharacWrite->Text));
			if(CbWriteTypes->ItemIndex == 10) AChar->SetValueAsDouble(StrToFloat(EdCharacWrite->Text));
			if(CbWriteTypes->ItemIndex == 11) AChar->SetValueAsSingle(StrToFloat(EdCharacWrite->Text));
			ADevice->WriteCharacteristic(AChar);
		}
		else {
			ShowMessage("This characteristic doesn''t allow Write");
        }
	}
	else {
		ShowMessage(EdCurrentDevice->Text + " is not available");
	}
}
//---------------------------------------------------------------------------
