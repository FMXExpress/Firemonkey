
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
#ifdef _WIN32
#include <tchar.h>
#endif
#if defined(__APPLE__) && defined(__arm__)
#include <iOSapi.UIKit.hpp>
#endif
#if defined __ANDROID__
#include <FMX.PLatform.Android.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#endif
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("MainFrm.cpp", BaseMainForm);
USEFORM("MainFrm_Phone.cpp", PhoneMainForm);
USEFORM("MainFrm_Tablet.cpp", TabletMainForm);
//---------------------------------------------------------------------------
#if defined(__APPLE__) && defined(__arm__)
bool IsTablet()
{
  return TUIDevice::Wrap(TUIDevice::OCClass->currentDevice())->userInterfaceIdiom() ==
	NativeUInt(UIUserInterfaceIdiomPad);
}
#endif
#if defined __ANDROID__
bool IsTablet()
{
	return (MainActivity()->getResources()->getConfiguration()->screenLayout & TJConfiguration::JavaClass->SCREENLAYOUT_SIZE_MASK)
		>= TJConfiguration::JavaClass->SCREENLAYOUT_SIZE_LARGE;
}
#endif
//---------------------------------------------------------------------------
extern "C" int FMXmain()
{
	try
	{
		Application->Initialize();
	    if (IsTablet()) {
			Application->CreateForm(__classid(TTabletMainForm), &TabletMainForm);
		} else {
			Application->CreateForm(__classid(TPhoneMainForm), &PhoneMainForm);
		}
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
