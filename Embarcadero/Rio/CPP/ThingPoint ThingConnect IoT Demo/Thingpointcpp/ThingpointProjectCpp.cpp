//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
#include <fmx.h>
#ifdef _WIN32
#include <tchar.h>
#endif
#pragma hdrstop
#include <System.StartUpCopy.hpp>
//---------------------------------------------------------------------------
USEFORM("NotifyDeviceFrameU.cpp", NotifyDeviceFrame); /* TFrame: File Type */
USEFORM("LoggingFrameU.cpp", EMSEdgeLoggingFrame); /* TFrame: File Type */
USEFORM("StreamingDeviceFrameU.cpp", StreamingDeviceFrame); /* TFrame: File Type */
USEFORM("ListenerFrameU.cpp", EMSEdgeModuleListenerFrame); /* TFrame: File Type */
USEFORM("ConnectionFrameU.cpp", EMSServerConnectionFrame); /* TFrame: File Type */
USEFORM("CacheDataModuleU.cpp", CacheDataModule); /* TDataModule: File Type */
USEFORM("MainFormU.cpp", EdgeMainForm);
USEFORM("EdgeServiceModuleU.cpp", EdgeServiceModule); /* TDataModule: File Type */
//---------------------------------------------------------------------------
extern "C" int FMXmain()
{
	try
	{
		Application->Initialize();
		Application->CreateForm(__classid(TCacheDataModule), &CacheDataModule);
		Application->CreateForm(__classid(TEdgeServiceModule), &EdgeServiceModule);
		Application->CreateForm(__classid(TEdgeMainForm), &EdgeMainForm);
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
