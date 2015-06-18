//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
#include <System.Math.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFrMainForm *FrMainForm;
//---------------------------------------------------------------------------
__fastcall TFrMainForm::TFrMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------


/*
  These functions have been translated from the examples provided by TE in its wiki
  http://processors.wiki.ti.com/index.php/SensorTag_User_Guide#Contactless_IR_Temperature_Sensor
*/

Integer shortSignedAtOffset(TBluetoothGattCharacteristic *ACharacteristic, Integer offset)
{
	Integer lowerByte;
	Integer upperByte;
	lowerByte = ACharacteristic->GetValueAsUInt8(offset);
	upperByte = ACharacteristic->GetValueAsInt8(offset + 1); // Note: interpret MSB as signed.
	return ((upperByte << 8) + lowerByte);
}

Integer shortUnsignedAtOffset(TBluetoothGattCharacteristic *ACharacteristic, Integer offset)
{
	Integer lowerByte;
	Integer upperByte;
	lowerByte = ACharacteristic->GetValueAsUInt8(offset);
	upperByte = ACharacteristic->GetValueAsUInt8(offset + 1); // Note: interpret MSB as unsigned.
	return (upperByte << 8) + lowerByte;
}


double extractAmbientTemperature(TBluetoothGattCharacteristic *ACharacteristic)
{
	return shortUnsignedAtOffset(ACharacteristic, 2) / 128.0;
}

double extractTargetTemperature(TBluetoothGattCharacteristic *ACharacteristic, double ambient )
{
	double S0 = 5.593E-14;	// Calibration factor
	double a1 = 1.75E-3;
	double a2 = -1.678E-5;
	double b0 = -2.94E-5;
	double b1 = -5.7E-7;
	double b2 = 4.63E-9;
	double c2 = 13.4;
	double Tref = 298.15;
	Integer twoByteValue;
	double Vobj2;
	double Tdie;
	double S;
	double Vos;
	double fObj;
	double tObj;

	twoByteValue = shortSignedAtOffset(ACharacteristic, 0);
	Vobj2 = twoByteValue;
	Vobj2 = Vobj2 * 0.00000015625;
	Tdie = ambient + 273.15;
	S = S0*( 1 + a1 * (Tdie - Tref) + a2 * Power((Tdie - Tref), 2));
	Vos = b0 + b1 *(Tdie - Tref) + b2 * Power((Tdie - Tref), 2);
	fObj = (Vobj2 - Vos) + c2 * Power((Vobj2 - Vos), 2);
	tObj = Power(Power(Tdie, 4) + (fObj / S), 0.25);

	return (tObj - 273.15);
}


void __fastcall TFrMainForm::BluetoothLE1CharacteristicRead(TObject * const Sender,
		  TBluetoothGattCharacteristic * const ACharacteristic, TBluetoothGattStatus AGattStatus)

{
	double LAmbient;
	double LTarget;
	double LAccel;
	Integer LHum;
	double LHHum;

	if (ACharacteristic->UUIDName == "UUID_IRT_DATA") {
		LAmbient = extractAmbientTemperature(ACharacteristic);
		LTarget = extractTargetTemperature(ACharacteristic, LAmbient);
		TVarRec vrAmbient[] = {LAmbient};
		EdAmbient->Text = Format("%f ºC", vrAmbient,1);
		TVarRec vrTarget[] = {LTarget};
		EdTarget->Text = Format("%f ºC",vrTarget,1);
	}
	else if (ACharacteristic->UUIDName == "UUID_ACC_DATA") {
		LAccel = ACharacteristic->GetValueAsInt8(0) / 64.0;
		TVarRec vrAccel1[] = {LAccel};
		EdAccelX->Text = Format("%f", vrAccel1, 1);
		LAccel = ACharacteristic->GetValueAsInt8(1) / 64.0;
		TVarRec vrAccel2[] = {LAccel};
		EdAccelY->Text = Format("%f",vrAccel2, 1);
		LAccel = ACharacteristic->GetValueAsInt8(2) / 64.0 * -1.0;
		TVarRec vrAccel3[] = {LAccel};
		EdAccelZ->Text = Format("%f",vrAccel3, 1);
	}
	else if (ACharacteristic->UUIDName == "UUID_HUM_DATA") {
		LHum = shortUnsignedAtOffset(ACharacteristic, 2);
		LHum = LHum - (LHum % 4);
		LHHum = (-6.0) + 125.0 * (LHum / 65535.0);
		TVarRec vrHum[] = {LHHum};
		EdHumidity->Text = Format("%f ", vrHum, 1);
	}
}
//---------------------------------------------------------------------------
void __fastcall TFrMainForm::BluetoothLE1EndDiscoverDevices(TObject * const Sender,
          TBluetoothLEDeviceList * const ADeviceList)
{
  TBluetoothLEDevice *LDevice;
  ListBox1->Items->Clear();
  for (int i = 0; i < ADeviceList->Count ; i++)
  {
  	LDevice = ADeviceList->Items[i];
	// asking for both names since some OS can provide a different name for the device
	if ((LDevice->DeviceName == "SensorTag") || (LDevice->DeviceName == "TI BLE Sensor Tag")) {
	  ListBox1->Items->AddObject(LDevice->DeviceName, LDevice); }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFrMainForm::BluetoothLE1EndDiscoverServices(TObject * const Sender,
		  TBluetoothGattServiceList * const AServiceList)
{
  Integer I;
  Integer J;
  for (I = 0; I < AServiceList->Count; I++)
  {
	if (AServiceList->Items[I]->UUIDName == "UUID_IRT_SERV") // Temperature service
	{
	  for (J = 0;  J < AServiceList->Items[I]->Characteristics->Count; J++)
	  {
		if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_IRT_CONF")
		{
		  AServiceList->Items[I]->Characteristics->Items[J]->SetValueAsUInt8(1); // this is needed to start receiving data
		  BluetoothLE1->WriteCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]);
		}
		else if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_IRT_DATA") {
		  BluetoothLE1->SubscribeToCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]); }
	  }
	}
	else if (AServiceList->Items[I]->UUIDName == "UUID_ACC_SERV") // Accelerometer service
	{
	  for (J = 0; J < AServiceList->Items[I]->Characteristics->Count; J++)
	  {
		if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_ACC_CONF")
		{
		  AServiceList->Items[I]->Characteristics->Items[J]->SetValueAsUInt8(1); // this is needed to start receiving data
		  BluetoothLE1->WriteCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]);
		}
		else if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_ACC_DATA")  {
		  BluetoothLE1->SubscribeToCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]);}
	  }
	}
	else if (AServiceList->Items[I]->UUIDName == "UUID_HUM_SERV")
	{
	  for (J = 0; J < AServiceList->Items[I]->Characteristics->Count; J++)
	  {
		if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_HUM_CONF")
		{
		  AServiceList->Items[I]->Characteristics->Items[J]->SetValueAsUInt8(1); // this is needed to start receiving data
		  BluetoothLE1->WriteCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]);
		}
		else if (AServiceList->Items[I]->Characteristics->Items[J]->UUIDName == "UUID_HUM_DATA") {
		  BluetoothLE1->SubscribeToCharacteristic(FcurrentDevice, AServiceList->Items[I]->Characteristics->Items[J]);}
	  }
	}
  }

}
//---------------------------------------------------------------------------
void __fastcall TFrMainForm::Button1Click(TObject *Sender)
{
  BluetoothLE1->DiscoverDevices(4000);
}
//---------------------------------------------------------------------------
void __fastcall TFrMainForm::Button2Click(TObject *Sender)
{
  if (ListBox1->ItemIndex >= 0)
  {
	FcurrentDevice = (TBluetoothLEDevice *) (ListBox1->Items->Objects[ListBox1->ItemIndex]);
	BluetoothLE1->DiscoverServices(FcurrentDevice);
  }
  else
	ShowMessage("Please select a device from the list");
}
//---------------------------------------------------------------------------
