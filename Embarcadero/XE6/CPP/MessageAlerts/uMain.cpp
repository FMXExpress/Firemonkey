
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
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::btnOneButtonAlertClick(TObject *Sender)
{
  // Show a standard alert with a single OK button
  ShowMessage("Hello World!");
}
//---------------------------------------------------------------------------

void __fastcall TForm2::btnMultipleButtonAlertClick(TObject *Sender)
{
	/* Show a multiple-button alert that triggers different code blocks according to
		your input */
	switch (MessageDlg("Choose a button:", TMsgDlgType::mtInformation,
		TMsgDlgButtons() << TMsgDlgBtn::mbYes << TMsgDlgBtn::mbNo << TMsgDlgBtn::mbCancel , 0))
	{
		case mrYes :
			ShowMessage("You choose Yes");
			break;
		case mrNo:
			ShowMessage("You choose No");
			break;
		case mrCancel:
			ShowMessage("You choose Cancel");
			break;
	}
}
//---------------------------------------------------------------------------

