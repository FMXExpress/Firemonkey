
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
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
#include <iOSapi.UIKit.hpp>
#endif
#pragma hdrstop

#include "uMain.h"
#if defined(__ANDROID__)
#include <Androidapi.JNI.Os.hpp>
#include <Androidapi.Helpers.hpp>
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
#ifdef _WIN32
	btnGetDeviceInfo->Visible = false;
#else
	btnGetDeviceInfo->Visible = true;
#endif
}
#if defined(__ANDROID__)
String GetCodename(String VerString)
{
  if (VerString == "1.0")
	return "BASE";
  else if (VerString == "1.1")
	return "BASE_1_1";
  else if (VerString == "1.5")
	return "CUPCAKE";
  else if (VerString == "1.6")
	return "DONUT";
  else if (VerString == "2.0")
	return "ECLAIR";
  else if (VerString == "2.0.1")
	return "ECLAIR_0_1";
  else if (VerString == "2.1")
	return "ECLAIR_MR1";
  else if (VerString == "2.2")
	return "FROYO";
  else if (VerString == "2.3")
	return "GINGERBREAD";
  else if (VerString == "2.3.3")
	return "GINGERBREAD_MR1";
  else if (VerString == "3.0")
	return "HONEYCOMB";
  else if (VerString == "3.1")
	return "HONEYCOMB_MR1";
  else if (VerString == "3.2")
	return "HONEYCOMB_MR2";
  else if (VerString == "4.0")
	return "ICE_CREAM_SANDWICH";
  else if (VerString == "4.0.3")
	return "ICE_CREAM_SANDWICH_MR1";
  else if (VerString == "4.1")
	return "JELLY_BEAN";
  else if (VerString == "4.2")
	return "JELLY_BEAN_MR1";
  else if (VerString == "4.3")
	return "JELLY_BEAN_MR2";
  else if (Pos("4.4", VerString) == 1)
	return "KITKAT";
  else return "UNKNOWN";
};
#endif
//---------------------------------------------------------------------------
void __fastcall TForm2::btnGetDeviceInfoClick(TObject *Sender)
{
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
	_di_UIDevice device = TUIDevice::Wrap(TUIDevice::OCClass->currentDevice());
	lbOSName->Text = lbOSName->Text.sprintf(L"OS Name: %s", device->systemName()->UTF8String());
	lbOSVersion->Text = lbOSVersion->Text.sprintf(L"OS Version: %s", device->systemVersion()->UTF8String());
	lbDeviceType->Text = lbDeviceType->Text.sprintf(L"Device Type: %s", device->model()->UTF8String());
#endif
#if defined(__ANDROID__)
  lbDeviceType->Text = String().sprintf(L"Device Type: %s", UTF8String(JStringToString(TJBuild::JavaClass->MODEL)).c_str());
  lbOSName->Text =     String().sprintf(L"OS Name: %s",  UTF8String(GetCodename( JStringToString(TJBuild_VERSION::JavaClass->RELEASE))).c_str());
  lbOSVersion->Text =  String().sprintf(L"OS Version: %s", UTF8String(JStringToString(TJBuild_VERSION::JavaClass->RELEASE)).c_str());
#endif
}
//---------------------------------------------------------------------------


