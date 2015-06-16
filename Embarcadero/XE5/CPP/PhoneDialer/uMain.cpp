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
		lblCarrierName->Text = Format("Carrier Name: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetCarrierName())));
		lblISOCountryCode->Text = Format("ISO Country Code: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetIsoCountryCode())));
		lblNetworkCode->Text = Format("Network Code: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetMobileCountryCode())));
		lblMobileNetwork->Text = Format("Mobile Network: %s", ARRAYOFCONST((PhoneDialerService->GetCarrier()->GetMobileNetwork())));
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
}
//---------------------------------------------------------------------------

