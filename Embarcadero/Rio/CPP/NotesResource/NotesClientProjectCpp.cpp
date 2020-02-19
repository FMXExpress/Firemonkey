//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif
#pragma hdrstop
#include <System.StartUpCopy.hpp>
//---------------------------------------------------------------------------
USEFORM("NotesAdapterModuleU.cpp", NotesAdapterModule); /* TDataModule: File Type */
USEFORM("NotesClientModuleU.cpp", NotesClientModule); /* TDataModule: File Type */
USEFORM("NotesClientFormU.cpp", NotesClientForm);
//---------------------------------------------------------------------------
extern "C" int FMXmain()
{
	try
	{
		Application->Initialize();
		Application->CreateForm(__classid(TNotesAdapterModule), &NotesAdapterModule);
		Application->CreateForm(__classid(TNotesClientModule), &NotesClientModule);
 		Application->CreateForm(__classid(TNotesClientForm), &NotesClientForm);
		Application->Run();
	}
	catch (System::Sysutils::Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw System::Sysutils::Exception("");
		}
		catch (System::Sysutils::Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
