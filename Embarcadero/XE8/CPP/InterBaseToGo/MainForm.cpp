// ---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

#include <fmx.h>
#include <System.UITypes.hpp>
#pragma hdrstop

#include "MainForm.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Change(TObject *Sender) {
	try {
		switch (TOSVersion::Platform) {
		case TOSVersion::TPlatform::pfWindows:
			IBDatabase1->DatabaseName =
				"C:\\Users\\Public\\Documents\\Embarcadero\\Studio\\15.0\\Samples\\Data\\employee.gdb";
			break;
		case TOSVersion::TPlatform::pfMacOS:
			IBDatabase1->DatabaseName = GetCurrentDir() + PathDelim + "employee.gdb";
			break;
		}

		if (CheckBox1->IsChecked) {
			IBDatabase1->Connected = true;
			IBDataSet1->Active = true;
			IBDataSet2->Active = true;
		}
		else {
			IBDatabase1->Connected = false;
			IBDataSet1->Active = false;
			IBDataSet2->Active = false;

		}
	}
	catch (Exception& E) {
		MeLog->Lines->Add(E.ClassName() + ": " + E.Message);
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::NavigatorBindSourceDB1Click(TObject *Sender,
	TNavigateButton Button)

{
	IBDataSet2->Active = false;
	IBDataSet2->ParamByName("Cust_No")->Value =
		IBDataSet1->FieldByName("CUST_NO")->Value;
	IBDataSet2->Active = true;

}
// ---------------------------------------------------------------------------
