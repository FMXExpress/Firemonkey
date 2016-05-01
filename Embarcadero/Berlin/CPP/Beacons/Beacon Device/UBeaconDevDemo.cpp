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

#include "UBeaconDevDemo.h"
#include <limits.h>
#include <string.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm8 *Form8;
//---------------------------------------------------------------------------
__fastcall TForm8::TForm8(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm8::SetNamespaceGeneration()
{
	BeaconDevice1->NamespaceGenerator = TNamespaceGeneratorMethod(CbbNamespaceGeneration->ItemIndex);
}
//---------------------------------------------------------------------------
bool __fastcall TForm8::CheckUUID()
{
	try
	{
		FGuid = StringToGUID("{" + EdtBeaconUUID->Text + "}");
		return True;
	}
	catch (EConvertError &e)
	{
	  ShowMessage(EdtBeaconUUID->Text + " is not a valid UUID value");
	  return False;
	}
}
//---------------------------------------------------------------------------
bool __fastcall TForm8::CheckValues()
{
	switch (BeaconDevice1->BeaconType) {
		case TBeaconDeviceMode::Standard:
		case TBeaconDeviceMode::Alternative:
			if (!CheckUUID()) {
				return False;
			}
			break;
		case TBeaconDeviceMode::EddystoneUID:
			SetNamespaceGeneration();
			switch (BeaconDevice1->NamespaceGenerator) {
				case ngNone:
				case ngHashFQDN:
					if (EdtEddyNamespace->Text == "") {
						ShowMessage("Please, enter a valid Namespace");
						EdtEddyNamespace->SetFocus();
						return False;
					}
					break;
				case ngElidedUUID:
					if (!CheckUUID()) {
						return False;
					}
					break;
			}
			if (EdtEddyInstance->Text == "") {
				ShowMessage("Please, enter a valid Instance value");
				EdtEddyNamespace->SetFocus();
				return False;
			}
			break;
		case TBeaconDeviceMode::EddystoneURL:
			if (EdtEddyURL->Text == "") {
				ShowMessage("Please, enter a URL");
				EdtEddyURL->SetFocus();
				return False;
			}
			break;
	}

	FMajor = StrToIntDef(EdtBeaconMajor->Text, 0);
	EdtBeaconMajor->Text = IntToStr(FMajor);
	FMinor = StrToIntDef(EdtBeaconMinor->Text, 0);
	EdtBeaconMinor->Text = IntToStr(FMinor);
	FTxPower = StrToIntDef(EdTxPower->Text, -56);
	EdTxPower->Text = IntToStr(FTxPower);
	FEddyNamespace = EdtEddyNamespace->Text;
	FEddyInstance = EdtEddyInstance->Text;
	FEddyURL = EdtEddyURL->Text;
	return True;
}
//---------------------------------------------------------------------------
void __fastcall TForm8::BtnRandomClick(TObject *Sender)
{
	TGUID LGuid;
	CreateGUID(LGuid);
	UnicodeString LStrGuid = GUIDToString(LGuid);
	EdtBeaconUUID->Text = LStrGuid.SubString0(1, LStrGuid.Length() - 2);
	if (EdtBeaconMajor->Enabled)
	{
		EdtBeaconMajor->Text = IntToStr(Random(USHRT_MAX));
		EdtBeaconMinor->Text = IntToStr(Random(USHRT_MAX));
	}
}

//---------------------------------------------------------------------------
String __fastcall TForm8::RandomHexString(int ABytesCount)
{
	char HEX_CHARS[16] = {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};
	String cad = "";
	for (int i = 1; i < (ABytesCount * 2); i++) {
		cad = cad + HEX_CHARS[Random(16)];
	}
	return cad;
}
//---------------------------------------------------------------------------
void __fastcall TForm8::BtnEnableBeaconClick(TObject *Sender) {
	if (BtnEnableBeacon->IsPressed)
	{
		if (CheckValues()) {
			BeaconDevice1->GUID = FGuid;
			BeaconDevice1->Major = FMajor;
			BeaconDevice1->Minor = FMinor;
			BeaconDevice1->TxPower = FTxPower;
			BeaconDevice1->EddystoneNamespace = FEddyNamespace;
			BeaconDevice1->EddystoneInstance = FEddyInstance;
			BeaconDevice1->EddystoneURL = FEddyURL;
		}
		else
		{
			BtnEnableBeacon->IsPressed = False;
		}
	}

	try
	{
		BeaconDevice1->Enabled = BtnEnableBeacon->IsPressed;
	}
	catch (EConvertError &e)
	{
	  ShowMessage("Failed to start beacon: " + e.Message);
	  BeaconDevice1->Enabled = False;
	  BtnEnableBeacon->IsPressed = False;
	}

	if (BtnEnableBeacon->IsPressed)
		BtnEnableBeacon->Text = "Disable Beacon";
	else
		BtnEnableBeacon->Text = "Enable Beacon";

	ImageControl1->Visible = BeaconDevice1->Enabled;
	Animation->Enabled = BeaconDevice1->Enabled;
	PnlBeaconInfo->Enabled = !BeaconDevice1->Enabled;
	PnlEddyInfo->Enabled = !BeaconDevice1->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TForm8::CbbBeaconTypeChange(TObject *Sender)
{
	if (CbbBeaconType->Selected->Text == "Standard") {
		BeaconDevice1->BeaconType = TBeaconDeviceMode::Standard;
	}else if (CbbBeaconType->Selected->Text == "Alternative") {
		BeaconDevice1->BeaconType = TBeaconDeviceMode::Alternative;
	}else if (CbbBeaconType->Selected->Text == "EddystoneUID") {
		BeaconDevice1->BeaconType = TBeaconDeviceMode::EddystoneUID;
	}else if (CbbBeaconType->Selected->Text == "EddystoneURL") {
		BeaconDevice1->BeaconType = TBeaconDeviceMode::EddystoneURL;
	}

	PnlEddyInfo->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneUID) ||
		(BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneURL);
	EdtEddyURL->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneURL);
	EdtEddyNamespace->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneUID);
	BtnRandomNamespace->Enabled = EdtEddyNamespace->Enabled;
	EdtEddyInstance->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneUID);
	CbbNamespaceGeneration->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneUID);
	EdtBeaconUUID->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::Standard) ||
		(BeaconDevice1->BeaconType == TBeaconDeviceMode::Alternative) ||
		(BeaconDevice1->BeaconType == TBeaconDeviceMode::EddystoneUID);
	EdtBeaconMajor->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::Standard) ||
		(BeaconDevice1->BeaconType == TBeaconDeviceMode::Alternative);
	EdtBeaconMinor->Enabled = (BeaconDevice1->BeaconType == TBeaconDeviceMode::Standard) ||
		(BeaconDevice1->BeaconType == TBeaconDeviceMode::Alternative);
}
//---------------------------------------------------------------------------

void __fastcall TForm8::FormCreate(TObject *Sender)
{
#ifndef __APPLE__
  CbbBeaconType->Items->Add("Alternative");
#endif
#ifdef __ANDROID__
  CbbBeaconType->Items->Add("EddystoneUID");
  CbbBeaconType->Items->Add("EddystoneURL");
#endif
#ifndef __ANDROID__
  PnlEddyInfo->Visible = False;
#endif
  CbbBeaconTypeChange(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TForm8::BtnRandomNamespaceClick(TObject *Sender)
{
	EdtEddyNamespace->Text = RandomHexString(10);
	EdtEddyInstance->Text = RandomHexString(6);
}
//---------------------------------------------------------------------------

