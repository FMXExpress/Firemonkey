//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "ProxClient.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TfrmProximityForm *frmProximityForm;
String ProximityDeviceName;
const String CProximityDeviceName = u"LINK LOSS";

const String ServiceUUID = _D("");
const String CharactUUID = _D("");

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

	FBLEManager = TBluetoothLEManager::Current;
	FBLEManager->OnDiscoveryEnd = DoDiscoveryEndEvent;
	FCurrentPosition = poUnknown;
	ProximityDeviceName = CProximityDeviceName;
	DoScan();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::btnConnectClick(TObject *Sender)
{
	if (FBLEDevice->Services->Count == 0)
		Memo1->Lines->Add("No services found!");
  else
	GetServiceAndCharacteristics();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::btnScanClick(TObject *Sender)
{
  btnConnect->Enabled = False;
  DoScan();
}
//---------------------------------------------------------------------------

void __fastcall TfrmProximityForm::CheckDistanceThreshold(int PathLoss)
{
  if (PathLoss < 25)
	SetPosition(poNear);
  else
	if (PathLoss < 45)
	  SetPosition(poFar);
	else
	  SetPosition(poSoFar);
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
	  if (LValue > 20) LValue = LValue - 255;
	  FTxPowerValue = LValue;
	}
	Memo1->Lines->Add("FTxPowerValue=" + IntToStr(FTxPowerValue) );
  }

  if (ACharacteristic->UUID == ALERT_LEVEL_CHARACTERISTIC)
  {
	if ( (ACharacteristic->GetService()->UUID == LINK_LOSS_SERVICE) && (ACharacteristic->Value.get_length()) > 0)
	{
	  LValue = ACharacteristic->Value[0];
	  lblLinkLossAlert->Text = IntToStr(LValue);
	  Memo1->Lines->Add("Link Loss Alert=" + IntToStr(LValue));
	}
  }

}

void __fastcall TfrmProximityForm::DoCharacteristicWrite(TObject *Sender,
									  TBluetoothGattCharacteristic *ACharacteristic,  TBluetoothGattStatus *AGattStatus)
{
  //Memo1.Lines.Add('Charact ' + ACharacteristic.UUIDName + ' writed! Value: ' + ACharacteristic.Value[0].ToString );
}


void __fastcall TfrmProximityForm::DoDiscoveryEndEvent(TObject *Sender, TBluetoothLEDeviceList *ADeviceList)
{
  int I;

  // log
  Memo1->Lines->Add(IntToStr(ADeviceList->Count) +  " devices discovered:");
  for (I = 0; I <= ADeviceList->Count - 1; I++)
	Memo1->Lines->Add(ADeviceList->Items[I]->DeviceName);

  FBLEDevice = NULL;
  for (I = 0; I <= FBLEManager->LastDiscoveredDevices->Count - 1; I++)
  {
	  if (FBLEManager->LastDiscoveredDevices->Items[I]->DeviceName == ProximityDeviceName)
	  {
		FBLEDevice = FBLEManager->LastDiscoveredDevices->Items[I];
		break;
	  }
  }

  if (FBLEDevice == NULL)
	lblDevice->Text = "Device not found";
  else
  {
	lblDevice->Text = ProximityDeviceName;
	FBLEDevice->OnServicesDiscovered = DoServicesDiscovered;
	FBLEDevice->OnCharacteristicRead = (TGattCharacteristicEvent) & DoCharacteristicRead;
	FBLEDevice->OnReadRSSI = (TGattDeviceRSSIEvent) & DoReadRSSI;
	FBLEDevice->OnCharacteristicWrite = (TGattCharacteristicEvent) & DoCharacteristicWrite;
	FServicesDiscovered = False;
	FBLEDevice->DiscoverServices();
//{$IFDEF MACOS}
//    //Wait for OnServicesDiscovered event
//    WaitTime := 8;
//    repeat
//      Application.ProcessMessages;
//      Sleep(500);
//      Dec(WaitTime)
//    until (FServicesDiscovered) or (WaitTime=0);
//{$ENDIF}
//
//    if FBLEDevice.Services.Count = 0 then begin
//      Memo1.Lines.Add('No services found!');
//    end
//    else
//      GetServiceAndCharacteristics;
  }
}

void __fastcall TfrmProximityForm::tmrReadRSSITimer(TObject *Sender)
{
  FBLEDevice->ReadRemoteRSSI();
}
//---------------------------------------------------------------------------
void __fastcall TfrmProximityForm::DoReadRSSI(TObject *Sender, int ARssiValue, TBluetoothGattStatus *AGattStatus)
{
	int LRatioDB;
	double LRatioLinear, LDistance;
  //Discard wrong values
  if (ARssiValue > 0) return;

  lblTxPower->Text = IntToStr(FTxPowerValue);
  lblRSSI->Text = IntToStr(ARssiValue) + ' dBm';
  LRatioDB = FTxPowerValue - ARssiValue;
  LRatioLinear = pow(10, LRatioDB / 10);
  LDistance = Sqrt(LRatioLinear);

  lblDistance->Text = FloatToStr(LDistance);
  lblDist2->Text = IntToStr(FTxPowerValue - ARssiValue);

  CheckDistanceThreshold(FTxPowerValue - ARssiValue);
}

void __fastcall TfrmProximityForm::DoScan()
{
  lblDevice->Text = "";
  FBLEManager->StartDiscovery(1500);
}

void __fastcall TfrmProximityForm::DoServicesDiscovered(TObject *Sender, TBluetoothGattServiceList *AServiceList)
{
  FServicesDiscovered = True;
  btnConnect->Enabled = True;
}

void __fastcall TfrmProximityForm::EnableRSSIMonitorize(bool Enabled)
{
  tmrReadRSSI->Enabled = True;
}

void __fastcall TfrmProximityForm::GetServiceAndCharacteristics()
{
  int I, J, K;

  for (I = 0; I <= FBLEDevice->Services->Count - 1; I++)
  {
	Memo1->Lines->Add(FBLEDevice->Services->Items[I]->UUIDName + " : " + GUIDToString(FBLEDevice->Services->Items[I]->UUID));
	for (J = 0; J <= FBLEDevice->Services->Items[I]->Characteristics->Count - 1; J++)
	{
	  Memo1->Lines->Add("--> " + FBLEDevice->Services->Items[I]->Characteristics->Items[J]->UUIDName + " : " +
						GUIDToString(FBLEDevice->Services->Items[I]->Characteristics->Items[J]->UUID));
	  for (K = 0; K <= FBLEDevice->Services->Items[I]->Characteristics->Items[J]->Descriptors->Count - 1; K++)
	  {
		Memo1->Lines->Add("----> " + FBLEDevice->Services->Items[I]->Characteristics->Items[J]->Descriptors->Items[K]->UUIDName + " : " +
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
	  Memo1->Lines->Add("Service found");
	  FLinkLossAlertLevelCharact = FLinkLossService->GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
	  FBLEDevice->ReadCharacteristic(FLinkLossAlertLevelCharact);
  }

  FImmediateAlertService = FBLEDevice->GetService(IMMEDIATE_ALERT_SERVICE);
  if (FImmediateAlertService != NULL)
  {
	  Memo1->Lines->Add("Service found");
	  FImmediateAlertLevelCharact = FImmediateAlertService->GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
	  FBLEDevice->ReadCharacteristic(FImmediateAlertLevelCharact);
  }

  FTXPowerService = FBLEDevice->GetService(TX_POWER_SERVICE);
  if (FTXPowerService != NULL)
  {
	  Memo1->Lines->Add("Service found");
	  FTXPowerLevelCharact = FTXPowerService->GetCharacteristic(TX_POWER_LEVEL_CHARACTERISTIC);
	  FTxPowerValue = 99; // Invalid value
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
			if ( (FNearCount >= 2) && (FCurrentPosition != poNear)) UpdateCurrentPosition(poNear);
			break;
		case poFar:
			FNearCount = 0;
			FFarCount = Min(FFarCount + 1, 2);
			FSoFarCount = 0;
			if ((FFarCount >= 2) && (FCurrentPosition != poFar)) UpdateCurrentPosition(poFar);
			break;
		case poSoFar:
			FNearCount = 0;
			FFarCount = 0;
			FSoFarCount = Min(FSoFarCount + 1, 2);
			if ((FSoFarCount >= 2) && (FCurrentPosition != poSoFar)) UpdateCurrentPosition(poSoFar);
			break;
	}

}

void __fastcall TfrmProximityForm::UpdateCurrentPosition(Position_t Position)
{
  FCurrentPosition = Position;
  switch(Position)
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
	Memo1->Lines->Add("FImmediateAlertLevelCharact service : " + FImmediateAlertLevelCharact->GetService()->UUIDName);
	Memo1->Lines->Add("Immediate Alert_Level Charact set to " + IntToStr(AlertLevel));
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
		Memo1->Lines->Add("LinkLoss Alert_Level Charact set to " + IntToStr(AlertLevel));
	}
}
