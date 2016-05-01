//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ClientSettingsUH
#define ClientSettingsUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <map>

const String SETTINGSDBFILE = "settings.dat";  // do not localize

class TSettingsList {
private:
	std::map<String, int> FWidths;
	String FFileName;
public:
	__fastcall TSettingsList(String AFileName = "");
	__fastcall~TSettingsList(void){}
public:
	void __fastcall Clear(void);
	void __fastcall AddWidth(String AKey, int AWidth);
	bool __fastcall GetWidth(String AKey, int &AWidth);
	void __fastcall SaveToFile(String AFileName = "");
	void __fastcall LoadFromFile(String AFileName = "");
};
#endif
