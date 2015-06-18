//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif
#pragma hdrstop
#include <System.StartUpCopy.hpp>
//---------------------------------------------------------------------------
USEFORM("ctrlsdemofrm.cpp", frmCtrlsDemo);
USEFORM("aboutboxfrm.cpp", frmAbout);
//---------------------------------------------------------------------------
extern "C" int FMXmain()
{
	try
	{
		Application->Initialize();
		Application->CreateForm(__classid(TfrmCtrlsDemo), &frmCtrlsDemo);
//		Application->CreateForm(__classid(TfrmAbout), &frmAbout);
		Application->RegisterFormFamily(L"TForm", OPENARRAY(TComponentClass,
			(__classid(TfrmCtrlsDemo), __classid(TfrmAbout))));
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
