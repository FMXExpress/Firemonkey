//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm5 *Form5;
//---------------------------------------------------------------------------
__fastcall TForm5::TForm5(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm5::DoItButtonClick(TObject *Sender)
{
  String LockCommand;
  // set the lock based on the combobox
  if (ComboBox1->ListItems[ComboBox1->ItemIndex]->Text == "Unlock")
	LockCommand = '0';
  else
	LockCommand = '1';
  RESTRequestLockOperation->Resource =
	"data_request?id=action&DeviceNum=3&serviceId=urn:micasaverde-com:serviceId:DoorLock1&action=SetTarget&newTargetValue="
	+ LockCommand;
  Memo1->Lines->Add(
	"data_request?id=action&DeviceNum=3"
  );
  Memo1->Lines->Add(
	"&serviceId=urn:micasaverde-com:serviceId:DoorLock1"
  );
  Memo1->Lines->Add(
	"&action=SetTarget&newTargetValue="
	+ LockCommand
  );
  RESTRequestLockOperation->Execute;
}
//---------------------------------------------------------------------------
void __fastcall TForm5::VeraLiteSwitchSwitch(TObject *Sender)
{
  // VeraLite Switch - check for operation
  if (VeraLiteSwitch->IsChecked) {
	// Set IP address and execute REST Request
	RESTClient1->BaseURL =
	  "http://"
	  + VeraLiteEdit->Text
	  + ":3480";
	Memo1->Lines->Add(RESTClient1->BaseURL);
	DoItButton->Enabled = true;
  }
  else
	DoItButton->Enabled = false;
}
//---------------------------------------------------------------------------
