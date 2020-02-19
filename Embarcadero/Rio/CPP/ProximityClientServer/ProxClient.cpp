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

#include "ProxClient.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TfrmProximityForm *frmProximityForm;

TBluetoothUUID LINK_LOSS_SERVICE;
TBluetoothUUID IMMEDIATE_ALERT_SERVICE;
TBluetoothUUID TX_POWER_SERVICE;
TBluetoothUUID ALERT_LEVEL_CHARACTERISTIC;
TBluetoothUUID TX_POWER_LEVEL_CHARACTERISTIC;

//---------------------------------------------------------------------------
__fastcall TfrmProximityForm::TfrmProximityForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmProximityForm::FormShow(TObject *Sender)
{
  LINK_LOSS_SERVICE = StringToGUID("{00001803-0000-1000-8000-00805F9B34FB}");
  IMMEDIATE_ALERT_SERVICE = StringToGUID("{00001802-0000-1000-8000-00805F9B34FB}");
  TX_POWER_SERVICE = StringToGUID("{00001804-0000-1000-8000-00805F9B34FB}");
  ALERT_LEVEL_CHARACTERISTIC = StringToGUID("{00002A06-0000-1000-8000-00805F9B34FB}");
  TX_POWER_LEVEL_CHARACTERISTIC = StringToGUID("{00002A07-0000-1000-8000-00805F9B34FB}");
  FRssiValue = 0;
  FRatioDB = 0;
  FRatioLinear = 0.0;
  FDistance = 0.0;

  FBLEManager = TBluetoothLEManager::Current;
  FBLEManager->OnDiscoveryEnd = DoDiscoveryEndEvent;
  FCurrentPosition = poUnknown;
  DoScan();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::Connect()
{
  if (FBLEDevice->Services->Count == 0)
	AddTextToMemo("No services found!");
  else
	GetServiceAndCharacteristics();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::btnScanClick(TObject *Sender)
{
  EnableRSSIMonitorize(False);
  DoScan();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::CheckDistanceThreshold(int PathLoss)
{
  if (PathLoss < 25)
	SetPosition(poNear);
  else if (PathLoss < 45)
	SetPosition(poFar);
  else
	SetPosition(poSoFar);
}

void __fastcall TfrmProximityForm::OnDeviceDisconnect(TObject *Sender)
{
  FBLEDevice = NULL;
  EnableRSSIMonitorize(False);
  DoScan(); //Restore the connection
}

void __fastcall TfrmProximityForm::AddTextToMemo(UnicodeString S)
{
	FLastMessage = S;
	TThread::Synchronize(NULL, SynchronizedMemoAdd);
}

void __fastcall TfrmProximityForm::SynchronizedMemoAdd()
{
	Memo1->Lines->Add(FLastMessage);
}

void __fastcall TfrmProximityForm::DoCharacteristicRead(TObject *Sender, TBluetoothGattCharacteristic *ACharacteristic,
  TBluetoothGattStatus *AGattStatus)
{
  int LValue;

  if (ACharacteristic->UUID == TX_POWER_LEVEL_CHARACTERISTIC)
  {
	if ( ACharacteristic->Value.get_length() > 0)
	{
	  LValue = ACharacteristic->Value[0];
	  // SINT8 : Max=20 : Min -100
	  if (LValue > 20)
	  	LValue = LValue - 255;
	  FTxPowerValue = LValue;
	}
	AddTextToMemo("FTxPowerValue=" + IntToStr(FTxPowerValue) );
  }

  if (ACharacteristic->UUID == ALERT_LEVEL_CHARACTERISTIC)
  {
	if ( (ACharacteristic->GetService()->UUID == LINK_LOSS_SERVICE) && (ACharacteristic->Value.get_length()) > 0)
	{
	  LValue = ACharacteristic->Value[0];
	  lblLinkLossAlert->Text = IntToStr(LValue);
	  AddTextToMemo("Link Loss Alert=" + IntToStr(LValue));
	}
  }
}

void __fastcall TfrmProximityForm::DoDiscoveryEndEvent(TObject *Sender, TBluetoothLEDeviceList *ADeviceList)
{
  int I;
  TBluetoothLEDevice *LBLEDevice;

  AddTextToMemo(IntToStr(ADeviceList->Count) +  " devices discovered:");
  for (I = 0; I <= ADeviceList->Count - 1; I++)
	AddTextToMemo(ADeviceList->Items[I]->DeviceName);

  FBLEDevice = NULL;
  for (I = 0; I <= ADeviceList->Count - 1; I++)
  {
	LBLEDevice = ADeviceList->Items[I];
	LBLEDevice->DiscoverServices();
	if (LBLEDevice->GetService(TX_POWER_SERVICE) != NULL) {
	  FBLEDevice = LBLEDevice;
	  FBLEDevice->OnCharacteristicRead = (TGattCharacteristicEvent) & DoCharacteristicRead;
	  FBLEDevice->OnDisconnect = (System::Classes::TNotifyEvent) & OnDeviceDisconnect;
	  FBLEDevice->OnReadRSSI = (TGattDeviceRSSIEvent) & DoReadRSSI;
	  TThread::Synchronize(NULL, Connect);
	  break;
	}
  }
  TThread::Synchronize(NULL, CheckDeviceName);
}

void __fastcall TfrmProximityForm::CheckDeviceName()
{
  if (FBLEDevice == NULL)
	lblDevice->Text = "Device not found";
  else
	lblDevice->Text = FBLEDevice->DeviceName;
}

void __fastcall TfrmProximityForm::tmrReadRSSITimer(TObject *Sender)
{
  if (FBLEDevice != NULL) {
	FBLEDevice->ReadRemoteRSSI();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmProximityForm::DoReadRSSI(TObject *Sender, int ARssiValue, TBluetoothGattStatus AGattStatus)
{
  //Discard wrong values
  if (AGattStatus != TBluetoothGattStatus::Success || ARssiValue > 0)
  	return;

  FRssiValue = ARssiValue;
  FRatioDB = FTxPowerValue - ARssiValue;
  FRatioLinear = pow(10.0, FRatioDB / 10);
  FDistance = Sqrt(FRatioLinear);

  TThread::Synchronize(NULL, RefreshData);
  CheckDistanceThreshold(FRatioDB);
}

void __fastcall TfrmProximityForm::RefreshData()
{
  lblTxPower->Text = IntToStr(FTxPowerValue);
  lblRSSI->Text = IntToStr(FRssiValue) + " dBm";
  lblDistance->Text = FloatToStr(FDistance);
  lblDist2->Text = IntToStr(FRatioDB);
}

void __fastcall TfrmProximityForm::DoScan()
{
  EnableRSSIMonitorize(false);
  lblDevice->Text = "Scanning for devices";

  TBluetoothUUIDsList *LList = new TBluetoothUUIDsList;
  try
  {
	TBluetoothUUID uuids[3];
	uuids[0] = LINK_LOSS_SERVICE;
	uuids[1] = IMMEDIATE_ALERT_SERVICE;
	uuids[2] = TX_POWER_SERVICE;

	LList->AddRange(uuids,3);
	FBLEManager->StartDiscovery(1500, LList);
  }
  __finally
  {
      delete LList;
  }
}

void __fastcall TfrmProximityForm::EnableRSSIMonitorize(bool Enabled)
{
  tmrReadRSSI->Enabled = Enabled;
}

void __fastcall TfrmProximityForm::GetServiceAndCharacteristics()
{
  int I, J, K;

  for (I = 0; I <= FBLEDevice->Services->Count - 1; I++)
  {
	AddTextToMemo(FBLEDevice->Services->Items[I]->UUIDName + " : " + GUIDToString(FBLEDevice->Services->Items[I]->UUID));
	for (J = 0; J <= FBLEDevice->Services->Items[I]->Characteristics->Count - 1; J++)
	{
	  AddTextToMemo("--> " + FBLEDevice->Services->Items[I]->Characteristics->Items[J]->UUIDName + " : " +
						GUIDToString(FBLEDevice->Services->Items[I]->Characteristics->Items[J]->UUID));
	  for (K = 0; K <= FBLEDevice->Services->Items[I]->Characteristics->Items[J]->Descriptors->Count - 1; K++)
	  {
		AddTextToMemo("----> " + FBLEDevice->Services->Items[I]->Characteristics->Items[J]->Descriptors->Items[K]->UUIDName + " : " +
						  GUIDToString(FBLEDevice->Services->Items[I]->Characteristics->Items[J]->Descriptors->Items[K]->UUID));
	  }
	}
  }

  FLinkLossService = NULL;
  FTXPowerService = NULL;
  FImmediateAlertService = NULL;

  FTXPowerLevelCharact = NULL;
  FImmediateAlertLevelCharact = NULL;
  FLinkLossAlertLevelCharact = NULL;

  FLinkLossService = FBLEDevice->GetService(LINK_LOSS_SERVICE);

  if (FLinkLossService != NULL)
  {
	  AddTextToMemo("Service found");
	  FLinkLossAlertLevelCharact = FLinkLossService->GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
	  FBLEDevice->ReadCharacteristic(FLinkLossAlertLevelCharact);
  }

  FImmediateAlertService = FBLEDevice->GetService(IMMEDIATE_ALERT_SERVICE);
  if (FImmediateAlertService != NULL)
  {
	  AddTextToMemo("Service found");
	  FImmediateAlertLevelCharact = FImmediateAlertService->GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
	  FBLEDevice->ReadCharacteristic(FImmediateAlertLevelCharact);
  }

  FTXPowerService = FBLEDevice->GetService(TX_POWER_SERVICE);
  if (FTXPowerService != NULL)
  {
	  AddTextToMemo("Service found");
	  FTXPowerLevelCharact = FTXPowerService->GetCharacteristic(TX_POWER_LEVEL_CHARACTERISTIC);
	  FTxPowerValue = -50; // Invalid value
	  if (FTXPowerLevelCharact != NULL)
		  FBLEDevice->ReadCharacteristic(FTXPowerLevelCharact);
  }

  WriteLinkLossAlertLevel(0);
  EnableRSSIMonitorize(True);
}

void __fastcall TfrmProximityForm::SetPosition(Position_t Position)
{
  switch(Position)
  {
	case poUnknown:
	  break;
	case poNear:
	  FNearCount = Min(FNearCount + 1, 2);
	  FFarCount = 0;
	  FSoFarCount = 0;
	  if ( (FNearCount >= 2) && (FCurrentPosition != poNear))
		UpdateCurrentPosition(poNear);
	  break;
	case poFar:
	  FNearCount = 0;
	  FFarCount = Min(FFarCount + 1, 2);
	  FSoFarCount = 0;
	  if ((FFarCount >= 2) && (FCurrentPosition != poFar))
		UpdateCurrentPosition(poFar);
	  break;
	case poSoFar:
	  FNearCount = 0;
	  FFarCount = 0;
	  FSoFarCount = Min(FSoFarCount + 1, 2);
	  if ((FSoFarCount >= 2) && (FCurrentPosition != poSoFar))
	  	UpdateCurrentPosition(poSoFar);
	  break;
  }
}

void __fastcall TfrmProximityForm::UpdatePositionLabel()
{
  switch(FCurrentPosition)
  {
	case poUnknown:
		break;
	case poNear:
		lblPosition->Text = "Near";
		break;
	case poFar:
		lblPosition->Text = "Far";
		break;
	case poSoFar:
		lblPosition->Text = "So Far";
		break;
	}
}

void __fastcall TfrmProximityForm::UpdateCurrentPosition(Position_t Position)
{
  FCurrentPosition = Position;
  TThread::Synchronize(NULL, UpdatePositionLabel);
  WriteImmediateAlertLevel(Position);
}


void __fastcall TfrmProximityForm::WriteImmediateAlertLevel(System::Byte AlertLevel)
{
  TBytes LData;

  LData.set_length(1);
  LData[0] = AlertLevel;
  if (FImmediateAlertLevelCharact != NULL)
  {
	FImmediateAlertLevelCharact->Value = LData;
	FBLEDevice->WriteCharacteristic(FImmediateAlertLevelCharact);
	AddTextToMemo("FImmediateAlertLevelCharact service : " + FImmediateAlertLevelCharact->GetService()->UUIDName);
	AddTextToMemo("Immediate Alert_Level Charact set to " + IntToStr(AlertLevel));
  }
}

void __fastcall TfrmProximityForm::WriteLinkLossAlertLevel(System::Byte AlertLevel)
{
	TBytes	LData;
	LData.set_length(1);
	LData[0] = AlertLevel;
	if (FImmediateAlertLevelCharact != NULL)
	{
		FImmediateAlertLevelCharact->Value = LData;
		FBLEDevice->WriteCharacteristic(FImmediateAlertLevelCharact);
		AddTextToMemo("LinkLoss Alert_Level Charact set to " + IntToStr(AlertLevel));
	}
}
