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

#include "UHeartRateForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TfrmHeartMonitor *frmHeartMonitor;

const TBluetoothUUID HRSERVICE =
	StringToGUID("{0000180D-0000-1000-8000-00805F9B34FB}");
const TBluetoothUUID HRMEASUREMENT_CHARACTERISTIC =
	StringToGUID("{00002A37-0000-1000-8000-00805F9B34FB}");
const TBluetoothUUID BODY_SENSOR_LOCATION_CHARACTERISTIC =
	StringToGUID("{00002A38-0000-1000-8000-00805F9B34FB}");

const String BodySensorLocations[] =
	{"Other", "Chest", "Wrist", "Finger", "Hand", "Ear Lobe", "Foot"};

const int HR_VALUE_FORMAT_MASK        = 0x01;
const int SENSOR_CONTACT_STATUS_MASK  = 0x06;
const int ENERGY_EXPANDED_STATUS_MASK = 0x08;
const int RR_INTERVAL_MASK            = 0x10;

//---------------------------------------------------------------------------
String BytesToString(const TBytes &B)
{
	String StrReturn = "";
	if (B.Length > 0) {
		StrReturn = Format("%0.2X", ARRAYOFCONST((B[0])));
		for (int i = 0; i < B.High; i++) {
			StrReturn += Format(" %0.2X", ARRAYOFCONST((B[i])));
        }
	}
	return StrReturn;
}
//---------------------------------------------------------------------------
__fastcall TfrmHeartMonitor::TfrmHeartMonitor(TComponent* Owner)
	: TForm(Owner)
{
}

// ---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DoServicesDiscovered
	(System::TObject* const Sender,
	TBluetoothGattServiceList* const AServiceList) {
	FServicesDiscovered = true;
	if (AServiceList->Count == 0) {
		Memo1->Lines->Add("No services found!");
		lblBPM->Text = "No services found!";
	}
}

//---------------------------------------------------------------------------
THRMFlags * _fastcall TfrmHeartMonitor::GetFlags(System::Byte Data)
{
	THRMFlags * _return = new THRMFlags();
	_return->HRValue16bits = ((Data & HR_VALUE_FORMAT_MASK) == 1);
	int LValue = ((Data & SENSOR_CONTACT_STATUS_MASK) >> 1);
	switch (LValue) {
	case 2:
		_return->SensorContactStatus = NonDetected;
		break;
	case 3:
		_return->SensorContactStatus = Detected;
		break;
	default:
		_return->SensorContactStatus = NonSupported;
		break;
	}
	_return->EnergyExpended =
		(((Data & ENERGY_EXPANDED_STATUS_MASK) >> 3) == 1);
	_return->RRInterval = (((Data & RR_INTERVAL_MASK) >> 4) == 1);
	return _return;
}

void __fastcall TfrmHeartMonitor::ManageCharacteristicData
	(const TBluetoothGattCharacteristic *ACharacteristic) {
	if (const_cast<TBluetoothGattCharacteristic*>(ACharacteristic)
		->UUID == HRMEASUREMENT_CHARACTERISTIC) {
		DisplayHeartRateMeasurementData(
			const_cast<TBluetoothGattCharacteristic*>(ACharacteristic)->Value);

	}

	if (const_cast<TBluetoothGattCharacteristic*>(ACharacteristic)
		->UUID == BODY_SENSOR_LOCATION_CHARACTERISTIC) {
		DisplayBodySensorLocationData(const_cast<TBluetoothGattCharacteristic*>
			(ACharacteristic)->Value[0]);
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DisplayHeartRateMeasurementData(TBytes Data) {
	THRMFlags *Flags = GetFlags(Data[0]);
	int LBPM = 0;
	if (Flags->HRValue16bits) {
		LBPM = Data[1] + (Data[2] * 16);
	}
	else {
		LBPM = Data[1];
	}
	switch (Flags->SensorContactStatus) {
	case NonSupported:
		lblContactStatus->Text = "";
		break;
	case NonDetected:
		lblContactStatus->Text = "Sensor contact non detected";
		break;
	case Detected:
		lblContactStatus->Text = "Sensor contact detected";
		break;
	}
	if (Flags->SensorContactStatus == NonDetected) {
		ClearData();
	}
	else {
		lblBPM->Text = IntToStr(LBPM) + " bpm";
		imgHeart->Visible = !imgHeart->Visible;
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DisplayBodySensorLocationData(UInt8 Index) {
	if (Index > 6) {
		lblBodyLocation->Text = "";
	}
	else {
		lblBodyLocation->Text = "Sensor location: " +
			BodySensorLocations[Index];
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DoCharacteristicRead
	(System::TObject* const Sender,
	TBluetoothGattCharacteristic* const ACharacteristic,
	TBluetoothGattStatus AGattStatus) {
	if (AGattStatus != TBluetoothGattStatus::Success) {
		Memo1->Lines->Add("Error reading Characteristic " +
			ACharacteristic->UUIDName + ": " +
			IntToStr(static_cast<int>(AGattStatus)));
	}
	else {
		String LSValue = BytesToString(ACharacteristic->Value);
		Memo1->Lines->Add(ACharacteristic->UUIDName + " Value: " + LSValue);
		ManageCharacteristicData(ACharacteristic);
	}

}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::GetServiceAndCharacteristics(void) {
	TBluetoothGattCharacteristicList * CharList = NULL;
	TBluetoothGattDescriptorList * Descriptor = NULL;
	for (int i = 0; i < FBLEDevice->Services->Count; i++) {
		CharList = FBLEDevice->Services->Items[i]->Characteristics;
		Memo1->Lines->Add(FBLEDevice->Services->Items[i]->UUIDName + " : " +
			GUIDToString(FBLEDevice->Services->Items[i]->UUID));
		for (int j = 0; j < CharList->Count; j++) {
			Memo1->Lines->Add("--> " + CharList->Items[j]->UUIDName + " : " +
				GUIDToString(CharList->Items[j]->UUID));
			Descriptor = CharList->Items[j]->Descriptors;
			for (int k = 0; k < Descriptor->Count; k++) {
				Memo1->Lines->Add("--> " + Descriptor->Items[k]->UUIDName +
					" : " + GUIDToString(Descriptor->Items[k]->UUID));
			}
		}
	}
	FHRGattService = NULL;
	FHRMeasurementGattCharact = NULL;
	FBodySensorLocationGattCharact = NULL;

	FHRGattService = FBLEDevice->GetService(HRSERVICE);
	if (FHRGattService != NULL) {
		Memo1->Lines->Add("Service found");
		FHRMeasurementGattCharact =
			FHRGattService->GetCharacteristic(HRMEASUREMENT_CHARACTERISTIC);
		FBodySensorLocationGattCharact =
			FHRGattService->GetCharacteristic
			(BODY_SENSOR_LOCATION_CHARACTERISTIC);
	}
	else {
		Memo1->Lines->Add("Service not found");
		lblBPM->Text = "Service not found";
	}
	EnableHRMMonitorize(true);
	ReadBodySensorLocation();
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DoDiscoveryEndEvent
	(System::TObject* const Sender, TBluetoothLEDeviceList* const ADeviceList) {
	Memo1->Lines->Add(IntToStr(ADeviceList->Count) + " devices discovered:");
	for (int i = 0; i < ADeviceList->Count; i++) {
		Memo1->Lines->Add(ADeviceList->Items[0]->DeviceName);
	}
	FBLEDevice = NULL;
	if (ADeviceList->Count > 0) {
		FBLEDevice = ADeviceList->First();
	}
	if (FBLEDevice == NULL) {
		lblDevice->Text = "Device not found";
	}
	else {
		lblDevice->Text = FBLEDevice->DeviceName;
		FServicesDiscovered = false;
		FBLEDevice->DiscoverServices();
		if (FBLEDevice->Services->Count == 0) {
			Memo1->Lines->Add("No services found!");
			lblBPM->Text = "No services found!";
		}
		else {
			GetServiceAndCharacteristics();
		}
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::btnMonitorizeClick(TObject *Sender) {
	if (btnMonitorize->Text.SubString(0, 4) == "Stop") {
		EnableHRMMonitorize(false);
	}
	else {
		EnableHRMMonitorize(true);
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::ReadBodySensorLocation(void) {
	if (FBodySensorLocationGattCharact != NULL) {
		FBLEDevice->ReadCharacteristic(FBodySensorLocationGattCharact);
	}
	else {
		Memo1->Lines->Add("FBodySensorLocationGattCharact not found!!!");
		lblBodyLocation->Text = "Sensor location charact not found";
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DoDescriptorRead
	(System::TObject* const Sender, TBluetoothGattDescriptor* const ADescriptor,
	TBluetoothGattStatus AGattStatus) {
	if (AGattStatus != TBluetoothGattStatus::Success) {
		Memo1->Lines->Add("Error reading Characteristc " +
			ADescriptor->UUIDName + ": " +
			StrToInt(static_cast<int>(AGattStatus)));
	}
	else {
		String LSValue = BytesToString(ADescriptor->GetValue());
		Memo1->Lines->Add(ADescriptor->UUIDName + " Value: " + LSValue);
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::EnableHRMMonitorize(bool Enabled) {
	TBluetoothGattDescriptor * LDescriptor;

	if (FHRMeasurementGattCharact != NULL) {
#if defined(_WIN32) || defined(_WIN64)
		LDescriptor = FHRMeasurementGattCharact->Descriptors->Items[0];
		LDescriptor->Notification = Enabled;
		FBLEDevice->WriteDescriptor(LDescriptor);
#endif
		FBLEDevice->SetCharacteristicNotification(FHRMeasurementGattCharact,
			Enabled);

		if (Enabled) {
			btnMonitorize->Text = "Stop monitoring";
		}
		else {
			btnMonitorize->Text = "Start monitoring";
			ClearData();
		}
		btnMonitorize->Enabled = true;
	}
	else {
		Memo1->Lines->Add("HRM Characteristic not found");
		lblBPM->Text = "HRM Characteristic not found";
		btnMonitorize->Enabled = false;
	}
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::btnScanClick(TObject *Sender) {
	DoScan();
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::DoScan(void) {
	ClearData();
	lblDevice->Text = "";
	lblBodyLocation->Text = "";
	lblContactStatus->Text = "";
	GUID Services[1];
	Services[0] = HRSERVICE;
	BluetoothLE1->DiscoverDevices(2500, &Services[0], 0);
}

//---------------------------------------------------------------------------
void __fastcall TfrmHeartMonitor::ClearData(void) {
	lblBPM->Text = "? bpm";
	imgHeart->Visible = false;
}

void __fastcall TfrmHeartMonitor::BluetoothLE1EndDiscoverDevices
	(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList) {
	DoDiscoveryEndEvent(Sender, ADeviceList);
}
//---------------------------------------------------------------------------

void __fastcall TfrmHeartMonitor::BluetoothLE1DescriptorRead
	(TObject * const Sender, TBluetoothGattDescriptor * const ADescriptor,
	TBluetoothGattStatus AGattStatus) {
	DoDescriptorRead(Sender, ADescriptor, AGattStatus);
}
//---------------------------------------------------------------------------

void __fastcall TfrmHeartMonitor::BluetoothLE1CharacteristicRead
	(TObject * const Sender,
	TBluetoothGattCharacteristic * const ACharacteristic,
	TBluetoothGattStatus AGattStatus) {
	DoCharacteristicRead(Sender, ACharacteristic, AGattStatus);
}
//---------------------------------------------------------------------------
