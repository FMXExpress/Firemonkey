//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif
#include <iOSapi.UIKit.hpp>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("MainFrm.cpp", BaseMainForm);
USEFORM("MainFrm_Phone.cpp", PhoneMainForm);
USEFORM("MainFrm_Tablet.cpp", TabletMainForm);
//---------------------------------------------------------------------------
bool IsTablet()
{
  return TUIDevice::Wrap(TUIDevice::OCClass->currentDevice())->userInterfaceIdiom() ==
	UIUserInterfaceIdiomPad;
}
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
