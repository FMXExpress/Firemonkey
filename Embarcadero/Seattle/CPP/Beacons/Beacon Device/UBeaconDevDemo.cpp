//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
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
bool __fastcall TForm8::CheckValues()
{
	try
	{
		FGuid = StringToGUID("{" + EdtBeaconUUID->Text + "}");
	}
	catch (EConvertError &e)
	{
	  ShowMessage(EdtBeaconUUID->Text + " is not a valid UUID value");
	  return False;
	}
	FMajor = StrToIntDef(EdtBeaconMajor->Text, 0);
	EdtBeaconMajor->Text = IntToStr(FMajor);
	FMinor = StrToIntDef(EdtBeaconMinor->Text, 0);
	EdtBeaconMinor->Text = IntToStr(FMinor);
	FTxPower = StrToIntDef(EdTxPower->Text, -56);
	EdTxPower->Text = IntToStr(FTxPower);
	return True;
}
//---------------------------------------------------------------------------
void __fastcall TForm8::BtnRandomClick(TObject *Sender)
{
	TGUID LGuid;
	CreateGUID(LGuid);
	UnicodeString LStrGuid = GUIDToString(LGuid);
	EdtBeaconUUID->Text = LStrGuid.SubString0(1, LStrGuid.Length() - 2);
	EdtBeaconMajor->Text = IntToStr(Random(USHRT_MAX));
	EdtBeaconMinor->Text = IntToStr(Random(USHRT_MAX));
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
		}
		else
		{
			BtnEnableBeacon->IsPressed = False;
		}
	}

	if (BtnEnableBeacon->IsPressed)
		BtnEnableBeacon->Text = "Disable Beacon";
	else
		BtnEnableBeacon->Text = "Enable Beacon";

	BeaconDevice1->Enabled = BtnEnableBeacon->IsPressed;
	ImageControl1->Visible = BeaconDevice1->Enabled;
	Animation->Enabled = BeaconDevice1->Enabled;
	PnlBeaconInfo->Enabled = !BeaconDevice1->Enabled;
}
//---------------------------------------------------------------------------
