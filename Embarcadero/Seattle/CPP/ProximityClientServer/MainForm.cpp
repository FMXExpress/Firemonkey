// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainForm.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm4 *Form4;

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
		FLinkLossService = FGattServer->CreateService
			(StringToGUID("{00001803-0000-1000-8000-00805F9B34FB}"),
			TBluetoothServiceType::Primary);
		// (Name: 'Alert Level'; UUID:'{00002A06-0000-1000-8000-00805F9B34FB}'),
		/*
		 (Name: 'IMMEDIATE ALERT'; UUID:'{00001802-0000-1000-8000-00805F9B34FB}'),
		 (Name: 'LINK LOSS'; UUID:'{00001803-0000-1000-8000-00805F9B34FB}'),
		 (Name: 'TX POWER'; UUID:'{00001804-0000-1000-8000-00805F9B34FB}'),
		 */
		/*
		 0 “No Alert”
		 1 “Mild Alert”
		 2 “High Alert,”
		 */

		TBluetoothPropertyFlags LinkLossFlags;
		LinkLossFlags << TBluetoothProperty::Read << TBluetoothProperty::Write;

		FLinkLoss = FGattServer->CreateCharacteristic(FLinkLossService,
													StringToGUID("{00002A06-0000-1000-8000-00805F9B34FB}"),
													LinkLossFlags,
													"This service defines behavior when a link is lost between two devices");

		CharValue[0] = 0;
		FLinkLoss->Value = CharValue;

		FGattServer->AddService(FLinkLossService);
	}

	if (FImmediateAlertService == NULL) {
		FImmediateAlertService = FGattServer->CreateService(StringToGUID("{00001802-0000-1000-8000-00805F9B34FB}"),
																		TBluetoothServiceType::Primary);
		/*
		 <Enumeration key="0" value="No Alert" />
		 <Enumeration key="1" value="Mild Alert" />
		 <Enumeration key="2" value="High Alert" />
		 */
		TBluetoothPropertyFlags ImmediateAlertFlags;
		ImmediateAlertFlags << TBluetoothProperty::Read <<
			TBluetoothProperty::Write;

		FImmediateAlert = FGattServer->CreateCharacteristic
			(FImmediateAlertService,
			StringToGUID("{00002A06-0000-1000-8000-00805F9B34FB}"),
			ImmediateAlertFlags);

		CharValue[0] = 0; // 'No Alert'
		FImmediateAlert->Value = CharValue;

		FGattServer->AddService(FImmediateAlertService);
		/*
		 The value 0x12 is interpreted as +18dBm
		 The value 0xEE is interpreted as -18dBm

		 <Minimum>-100</Minimum>
		 <Maximum>20</Maximum>
		 */
	}

	if (FTxPowerService == NULL) {
		FTxPowerService = FGattServer->CreateService
			(StringToGUID("{00001804-0000-1000-8000-00805F9B34FB}"),
			TBluetoothServiceType::Primary);

		TBluetoothPropertyFlags TxPowerFlags;
		TxPowerFlags << TBluetoothProperty::Read;

		FTxPower = FGattServer->CreateCharacteristic(FTxPowerService,
			StringToGUID("{00002A07-0000-1000-8000-00805F9B34FB}"),
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
	TBluetoothLEDevice* ADevice) {
	ShowMessage("CONNECTED");
}

void __fastcall TForm4::UpdateTextAndSwitch() {
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
	TBluetoothGattStatus *AGattStatus) {
	UpdateTextAndSwitch();
	Memo1->Lines->Add("MyReadEvent " + ACharacteristic->UUIDName);
	// + ' Value: ' + BytesToString(ACharacteristic.Value));
}

void __fastcall TForm4::MyWriteEvent(TObject *Sender,
	TBluetoothGattCharacteristic *ACharacteristic,
	TBluetoothGattStatus *AGattStatus, TBytes AValue) {
	int IntValue;

	UpdateTextAndSwitch();
	Memo1->Lines->Add("MyWriteEvent " + ACharacteristic->UUIDName + " Value: " +
	BytesToString(AValue));

	if (ACharacteristic->UUID == StringToGUID
		("{00002A06-0000-1000-8000-00805F9B34FB}")) {
		IntValue = StrToInt(BytesToString(AValue));
		if (IntValue == 1) {
			txtALERT->Text = "MID ALERT";
			highAlertAnimation->Enabled = False;
			medAlertAnimation->Enabled = True;
		}
		else {
			if (IntValue == 2) {
				txtALERT->Text = "HIGH ALERT";
				medAlertAnimation->Enabled = False;
				highAlertAnimation->Enabled = True;
			}
			else if (IntValue == 0) {
				txtALERT->Text = "NO ALERT";
				medAlertAnimation->Enabled = False;
				highAlertAnimation->Enabled = False;
				Rectangle1->Fill->Color = TAlphaColor(TAlphaColorRec::Green);
			}
		}

	}
}

void __fastcall TForm4::tmrRSSITimer(TObject *Sender)
{
  if ( FBluetoothLEDevice == NULL)
	FBluetoothLEDevice->ReadRemoteRSSI();

}
//---------------------------------------------------------------------------

void __fastcall TForm4::tmrReviewDeviceConnectedTimer(TObject *Sender)
{
  FBluetoothManagerLE = TBluetoothLEManager::Current;
}
//---------------------------------------------------------------------------

