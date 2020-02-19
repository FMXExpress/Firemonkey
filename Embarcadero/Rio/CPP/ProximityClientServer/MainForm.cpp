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

#include "MainForm.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm4 *Form4;

//  (Name: 'Alert Level'; UUID:'{00002A06-0000-1000-8000-00805F9B34FB}'),
//  (Name: 'LINK LOSS'; UUID:'{00001803-0000-1000-8000-00805F9B34FB}'),
//  (Name: 'IMMEDIATE ALERT'; UUID:'{00001802-0000-1000-8000-00805F9B34FB}'),
//    0 "No Alert"
//    1 "Mild Alert"
//    2 "High Alert,"
//  (Name: 'TX POWER'; UUID:'{00001804-0000-1000-8000-00805F9B34FB}'),
//    The value 0x12 is interpreted as +18dBm
//    The value 0xEE is interpreted as -18dBm
//    <Minimum>-100</Minimum>
//    <Maximum>20</Maximum>

  TBluetoothUUID LINK_LOSS_SERVICE;
  TBluetoothUUID IMMEDIATE_ALERT_SERVICE;
  TBluetoothUUID TX_POWER_SERVICE;

  TBluetoothUUID ALERT_LEVEL_CHARACTERISTIC;
  TBluetoothUUID TX_POWER_LEVEL_CHARACTERISTIC;

// ---------------------------------------------------------------------------
__fastcall TForm4::TForm4(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm4::StartAnnounceClick(TObject *Sender) {
  Integer I;
  TBytes CharValue;
  TBluetoothGattCharacteristic *LCharacteristic;

  FBluetoothManagerLE = TBluetoothLEManager::Current;
  FGattServer = FBluetoothManagerLE->GetGattServer();
  FGattServer->OnConnectedDevice = DoOnConnectedDevice;
  FGattServer->OnCharacteristicRead =	(TGattCharacteristicReadEvent) & MyReadEvent;
  FGattServer->OnCharacteristicWrite = (TGattCharacteristicWriteEvent) & MyWriteEvent;
  Memo1->Lines->Add("[FGattServer] Started and announced");
  CharValue.set_length(1);

  if (FLinkLossService == NULL) {
	FLinkLossService = FGattServer->CreateService(LINK_LOSS_SERVICE, TBluetoothServiceType::Primary);
	TBluetoothPropertyFlags LinkLossFlags;
	LinkLossFlags << TBluetoothProperty::Read << TBluetoothProperty::Write;
	FLinkLoss = FGattServer->CreateCharacteristic(FLinkLossService, ALERT_LEVEL_CHARACTERISTIC,
	  LinkLossFlags, "This service defines behavior when a link is lost between two devices");

	CharValue[0] = 0;
	FLinkLoss->Value = CharValue;

	FGattServer->AddService(FLinkLossService);
  }

  if (FImmediateAlertService == NULL) {
	FImmediateAlertService = FGattServer->CreateService(IMMEDIATE_ALERT_SERVICE, TBluetoothServiceType::Primary);
	TBluetoothPropertyFlags ImmediateAlertFlags;
	ImmediateAlertFlags << TBluetoothProperty::Read << TBluetoothProperty::Write;
	FImmediateAlert = FGattServer->CreateCharacteristic(FImmediateAlertService, ALERT_LEVEL_CHARACTERISTIC,
	  ImmediateAlertFlags);

	CharValue[0] = 0; // 'No Alert'
	FImmediateAlert->Value = CharValue;

	FGattServer->AddService(FImmediateAlertService);
  }

  if (FTxPowerService == NULL) {
	FTxPowerService = FGattServer->CreateService(TX_POWER_SERVICE, TBluetoothServiceType::Primary);

	TBluetoothPropertyFlags TxPowerFlags;
	TxPowerFlags << TBluetoothProperty::Read;

	FTxPower = FGattServer->CreateCharacteristic(FTxPowerService, TX_POWER_LEVEL_CHARACTERISTIC,
	  TxPowerFlags, "Represents the current transmit power");

	CharValue[0] = 0xCD; // 'No Alert'
	FTxPower->Value = CharValue;
	FGattServer->AddService(FTxPowerService);
  }
  Memo1->Lines->Add("[FGattServer] Services created");
}
// ---------------------------------------------------------------------------

String __fastcall TForm4::BytesToString(TBytes B) {
  int I;
  String Result;
  TVarRec args[1];

  if (B.Length > 0) {
	args[0] = B[0];
	if (B.Length == 1)
	  Result = Format("%0.2X", args, 0);
	else
	  Result = Format("%0.2X ", args, 0);

	for (I = 1; I <= B.Length - 1; I++) {
	  args[0] = B[I];
	  Result = Result + Format(" %0.2X", args, 0);
	}
  }
  else
	 Result = "";
  return Result;
}

void __fastcall TForm4::DoOnConnectedDevice(TObject *Sender,
	TBluetoothLEDevice* ADevice)
{
  TThread::Synchronize(NULL, UpdateTextAndSwitch);
}

void __fastcall TForm4::UpdateTextAndSwitch()
{
  if (swConnected->IsChecked == False) {
	swConnected->IsChecked = True;
	lbDeviceConnected->FontColor = TAlphaColor(TAlphaColorRec::Green);
	lbDeviceConnected->Text = "Device connected";
	Rectangle1->Fill->Color = TAlphaColor(TAlphaColorRec::Green);
	Memo1->Lines->Add(lbDeviceConnected->Text);
  }
}

void __fastcall TForm4::MyReadEvent(TObject *Sender,
	TBluetoothGattCharacteristic *ACharacteristic,
	TBluetoothGattStatus *AGattStatus)
{
	UpdateTextAndSwitch();
	Memo1->Lines->Add("MyReadEvent " + ACharacteristic->UUIDName);
}

void __fastcall TForm4::MyWriteEvent(TObject *Sender,
	TBluetoothGattCharacteristic *ACharacteristic,
	TBluetoothGattStatus *AGattStatus, TBytes AValue)
{
  int IntValue;

  UpdateTextAndSwitch();
  Memo1->Lines->Add("MyWriteEvent " + ACharacteristic->UUIDName + " Value: " + BytesToString(AValue));

  if (ACharacteristic->UUID == ALERT_LEVEL_CHARACTERISTIC)
  {
	IntValue = StrToInt(BytesToString(AValue));
	if (IntValue == 1) {
	  lbALERT->Text = "MID ALERT";
	  highAlertAnimation->Enabled = False;
	  medAlertAnimation->Enabled = True;
	}
	else if (IntValue == 2) {
	  lbALERT->Text = "HIGH ALERT";
	  medAlertAnimation->Enabled = False;
	  highAlertAnimation->Enabled = True;
	}
	else if (IntValue == 0) {
	  lbALERT->Text = "NO ALERT";
	  medAlertAnimation->Enabled = False;
	  highAlertAnimation->Enabled = False;
	  Rectangle1->Fill->Color = TAlphaColor(TAlphaColorRec::Green);
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm4::FormShow(TObject *Sender)
{
  LINK_LOSS_SERVICE = StringToGUID("{00001803-0000-1000-8000-00805F9B34FB}");
  IMMEDIATE_ALERT_SERVICE = StringToGUID("{00001802-0000-1000-8000-00805F9B34FB}");
  TX_POWER_SERVICE = StringToGUID("{00001804-0000-1000-8000-00805F9B34FB}");
  ALERT_LEVEL_CHARACTERISTIC = StringToGUID("{00002A06-0000-1000-8000-00805F9B34FB}");
  TX_POWER_LEVEL_CHARACTERISTIC = StringToGUID("{00002A07-0000-1000-8000-00805F9B34FB}");
}
//---------------------------------------------------------------------------

