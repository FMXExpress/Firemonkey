
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "OSVersionForm.h"
#include <System.SysUtils.hpp>
#include <iostream>
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
using namespace std;
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------


void __fastcall TForm1::ButtonGetOSInfoClick(TObject *Sender) {

	MemoLog->Lines->Clear();

	MemoLog->Lines->Add(TOSVersion::ToString());
	MemoLog->Lines->Add(EmptyStr);


	switch (TOSVersion::Architecture) {
	case TOSVersion::TArchitecture::arIntelX86:
		MemoLog->Lines->Add("Architecture: IntelX86");
		break;
	case TOSVersion::TArchitecture::arIntelX64:
		MemoLog->Lines->Add("Architecture: IntelX64");
		break;
	}

	switch (TOSVersion::Platform) {
	case TOSVersion::TPlatform::pfWindows:
		MemoLog->Lines->Add("Platform: Windows");
		break;
	case TOSVersion::TPlatform::pfMacOS:
		MemoLog->Lines->Add("Platform: MacOS");
		break;
	}
	String i = static_cast<String>(TOSVersion::Build);

	MemoLog->Lines->Add(Format("Build: %d", ARRAYOFCONST((TOSVersion::Build))));
	MemoLog->Lines->Add(Format("Major: %d", ARRAYOFCONST((TOSVersion::Major))));
	MemoLog->Lines->Add(Format("Minor: %d", ARRAYOFCONST((TOSVersion::Minor))));

	MemoLog->Lines->Add(TOSVersion::Name);

	MemoLog->Lines->Add(Format("Service Pack - Minor: %d",
		ARRAYOFCONST((TOSVersion::ServicePackMinor))));
	MemoLog->Lines->Add(Format("Service Pack - Major: %d",
		ARRAYOFCONST((TOSVersion::ServicePackMajor))));

}
// ---------------------------------------------------------------------------
