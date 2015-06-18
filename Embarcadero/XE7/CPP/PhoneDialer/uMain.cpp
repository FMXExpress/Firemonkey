
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TPhoneDialerForm *PhoneDialerForm;
//---------------------------------------------------------------------------
__fastcall TPhoneDialerForm::TPhoneDialerForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPhoneDialerForm::btnGetCarrierInfoClick(TObject *Sender)
{
	_di_IFMXPhoneDialerService PhoneDialerService;
	/*  test whether the PhoneDialer services are supported */
	if(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXPhoneDialerService)) &&
		(PhoneDialerService = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXPhoneDialerService)))) {
		/* if yes, then update the labels with the retrieved information */

		CarrierNameItem->ItemData->Detail = Format("Carrier Name: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetCarrierName())));
		CountryCodeItem->ItemData->Detail = Format("ISO Country Code: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetIsoCountryCode())));
		NetworkCodeItem->ItemData->Detail = Format("Network Code: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetMobileCountryCode())));
		MobileNetworkItem->ItemData->Detail = Format("Mobile Network: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetMobileNetwork())));
	}
	else {
        ShowMessage("PhoneDialer service not supported");
    }

}
//---------------------------------------------------------------------------

void __fastcall TPhoneDialerForm::btnMakeCallClick(TObject *Sender)
{
	_di_IFMXPhoneDialerService PhoneDialerService;
	/*  test whether the PhoneDialer services are supported */
	if(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXPhoneDialerService)) &&
		(PhoneDialerService = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXPhoneDialerService)))) {
		/* if the Telephone Number is entered in the edit box then make the call, else
			display an error message */
		if (edtTelephoneNumber->Text != "") {
			PhoneDialerService->Call(edtTelephoneNumber->Text);
		}
		else {
			ShowMessage("Please type-in a telephone number.");
			edtTelephoneNumber->SetFocus();
		}
	}
	else {
        ShowMessage("PhoneDialer service not supported");
    }
}
//---------------------------------------------------------------------------

