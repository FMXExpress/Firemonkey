//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "FDownloadDemo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
//---------------------------------------------------------------------------
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::BStartDownloadClick(TObject *Sender)
{
  ((TButton *)Sender)->Enabled = false;
  SampleDownload();
  ((TButton *)Sender)->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormCreate(TObject *Sender)
{
  ButtonCancelArray[0] = Button1;
  ButtonCancelArray[1] = Button2;
  ButtonCancelArray[2] = Button3;
  ButtonCancelArray[3] = Button4;
  ProgressBarArray[0] = ProgressBarPart1;
  ProgressBarArray[1] = ProgressBarPart2;
  ProgressBarArray[2] = ProgressBarPart3;
  ProgressBarArray[3] = ProgressBarPart4;
  LabelProgressArray[0] = Label1;
  LabelProgressArray[1] = Label2;
  LabelProgressArray[2] = Label3;
  LabelProgressArray[3] = Label4;
}
//---------------------------------------------------------------------------
void TForm2::SampleDownload(void)
{
	const int NumThreads = 4;
	THTTPClient *LClient;
	LClient = THTTPClient::Create();

	try
	{
		String LFileName = EditFileName->Text;
		String LPath = System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetDocumentsPath(), LFileName);

		String URL = EditURL->Text;

		if (LClient->CheckDownloadResume(URL) == true)
		{
			_di_IHTTPResponse LResponse = LClient->Head(URL);

			// Get space for the file that is going to be dowloaded
			__int64 LSize = LResponse->ContentLength;
			System::Classes::TFileStream *STFile = new System::Classes::TFileStream(LFileName, fmCreate);
			try
			{
				STFile->Size = LSize;
			}
			__finally
			{
				STFile->Free();
			};


			// Split the file in four blocks
			__int64 LFragSize = LSize / NumThreads;
			__int64 LStart = 0;
			__int64 LEnd = LStart + LFragSize;

			DynamicArray<TDownloadThread *> LDownloadThreads;
			LDownloadThreads.Length = NumThreads;
			for (int i = 0; i < NumThreads; i++)
			{
				// Create the Thread
				LDownloadThreads[i] = new TDownloadThread(URL, LFileName, i, LStart, LEnd);
				LDownloadThreads[i]->OnThreadData = ReceiveThreadDataEvent;

				// Adjust the ProgressBar Max Value
				if (LEnd >= LSize)
				{
				  ProgressBarArray[i]->Max = LFragSize - (LEnd - LSize);
				  LEnd = LSize;
				}
				else
				  ProgressBarArray[i]->Max = LFragSize;

				ProgressBarArray[i]->Min = 0;
				ProgressBarArray[i]->Value = 0;
				ButtonCancelArray[i]->Enabled = True;
				LabelProgressArray[i]->Text = "0 KB/s";
				Application->ProcessMessages();

				// Update Start and End Values
				LStart = LStart + LFragSize;
				LEnd = LStart + LFragSize;
			}


			// Start the download process
			__int64 LStartTime = TThread::GetTickCount();
			for (int i = 0; i < NumThreads; i++)
				LDownloadThreads[i]->Start();

			// Wait until all threads finish
			for (Boolean LFinished = false; LFinished == false; )
			{
				Application->ProcessMessages();
				LFinished = True;
				for (int i = 0; i < NumThreads; i++)
				  LFinished = LFinished && LDownloadThreads[i]->Finished;
			}

			// Calculate elapsed time and Average Speed;
			__int64 LEndTime = TThread::GetTickCount() - LStartTime;
			TVarRec data[1];
			data[0] = (((LSize*1000) / LEndTime) / 1024);
			LabelGlobalSpeed->Text = Format("Global Speed: %d KB/s", data, 0);

			// Cleanup Threads
			for (int i = 0; i < NumThreads; i++)
				delete LDownloadThreads[i];
		}
	}
	__finally
	{
		LClient->Free();
	}
}
//---------------------------------------------------------------------------
TMyThreadProcedure::TMyThreadProcedure(TForm2 *AForm, int ThreadNo, int ASpeed, __int64 AContentLength,
	__int64 AReadCount, Boolean Abort): FForm(AForm), FThreadNo(ThreadNo), FSpeed(ASpeed),
	FContentLength(AContentLength), FReadCount(AReadCount), FAbort(Abort)
{
}
//---------------------------------------------------------------------------
void __fastcall TMyThreadProcedure::Invoke(void)
{
	FAbort = !FForm->ButtonCancelArray[FThreadNo]->Enabled;
	FForm->ProgressBarArray[FThreadNo]->Value = FReadCount;
	TVarRec LVarRec[1];
	LVarRec[0] = (FSpeed / 1024);
	FForm->LabelProgressArray[FThreadNo]->Text = Format("%d KB/s", LVarRec, 0);

	Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReceiveThreadDataEvent(const TObject *Sender, int ThreadNo, int ASpeed, __int64 AContentLength,
	__int64 AReadCount, Boolean &Abort)
{
	_di_TThreadProcedure mtp = new TMyThreadProcedure(this, ThreadNo, ASpeed, AContentLength, AReadCount, Abort);
	System::Classes::TThread::Synchronize(NULL, mtp);

	TMyThreadProcedure &a = (TMyThreadProcedure &)*mtp;
	Abort = a.FAbort;
//	delete mtp;
}
//---------------------------------------------------------------------------
__fastcall TDownloadThread::TDownloadThread(UnicodeString &AURL, UnicodeString &AFileName, int AThreadNo,
	__int64 StartPos, __int64 EndPos): TThread(true), FURL(AURL), FFileName(AFileName), FThreadNo(AThreadNo),
	FStartPos(StartPos), FEndPos(EndPos)
{
}
//---------------------------------------------------------------------------
void __fastcall TDownloadThread::Execute(void)
{
	THTTPClient *LHttpClient = THTTPClient::Create();
	try
	{
		LHttpClient->OnReceiveData = ReceiveDataEvent;
		TFileStream *LStream = new TFileStream(FFileName, fmOpenWrite | fmShareDenyNone);
		try
		{
		  FTimeStart = GetTickCount();
		  _di_IHTTPResponse LResponse = LHttpClient->GetRange(FURL, FStartPos, FEndPos, LStream);
		}
		__finally
		{
		  delete LStream;
		}
	}
	__finally
	{
		LHttpClient->Free();
	}
}
//---------------------------------------------------------------------------
void __fastcall TDownloadThread::ReceiveDataEvent(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount,
	Boolean &Abort)
{
	if (FOnThreadData != NULL)
	{
		unsigned int LTime = GetTickCount() - FTimeStart;
		__int64 LSpeed = (AReadCount * 1000) / LTime;
		FOnThreadData(Sender, FThreadNo, LSpeed, AContentLength, AReadCount, Abort);
	}
}

//---------------------------------------------------------------------------

void __fastcall TForm2::ButtonStopClick(TObject *Sender)
{
	((TButton *)Sender)->Enabled = false;
}
//---------------------------------------------------------------------------

