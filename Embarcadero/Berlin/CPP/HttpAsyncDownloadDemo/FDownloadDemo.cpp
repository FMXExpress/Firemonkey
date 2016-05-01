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
#pragma hdrstop

#include "FDownloadDemo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::BStartDownloadClick(TObject *Sender)
{
  BStartDownload->Enabled = false;
  SampleDownload();
}
//---------------------------------------------------------------------------
void __fastcall TForm2::BStopDownloadClick(TObject *Sender)
{
	BStopDownload->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::DoEndDownload(const System::Types::_di_IAsyncResult AsyncResult)
{
  try {
	FAsyncResponse = THTTPClient::EndAsyncHTTP(AsyncResult);
	System::Classes::TThread::Synchronize(NULL, SyncLambda<UnicodeString>(&this->LogText, "Download Finished"));
  }
  __finally {
	FDownloadStream->Free();
	BStopDownload->Enabled = false;
	BStartDownload->Enabled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm2::SampleDownload(void)
{
  UnicodeString LFileName = System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetDocumentsPath(), EditFileName->Text);
  try
  {
	FAsyncResponse = NULL;
	UnicodeString URL = EditURL->Text;

	_di_IHTTPResponse LResponse = FClient->Head(URL);
	__int64 LSize = LResponse->ContentLength;

	ProgressBarDownload->Max = LSize;
	ProgressBarDownload->Min = 0;
	ProgressBarDownload->Value = 0;
	LabelGlobalSpeed->Text = "Global speed: 0 KB/s";

	TVarRec LVarRec[3];
	LVarRec[0] = EditFileName->Text;
	LVarRec[1] = LSize;
	LVarRec[2] = LFileName;
	Memo1->Lines->Add(Format("Downloading: '%s' (%d Bytes) into '%s'" , LVarRec, 2));

	// Create the file that is going to be dowloaded
	FDownloadStream = new TFileStream(LFileName, fmCreate);
	FDownloadStream->Position = 0;

	FGlobalStart = TThread::GetTickCount();

	// Start the download process
	FAsyncResponse = FClient->BeginGet(DoEndDownload, URL, FDownloadStream);
  }
  __finally
  {
	BStopDownload->Enabled = FAsyncResponse != NULL;
	BStartDownload->Enabled = FAsyncResponse == NULL;
  }
}
//---------------------------------------------------------------------------
void TForm2::LogText(UnicodeString AText)
{
	Memo1->Lines->Add(AText);
}
//---------------------------------------------------------------------------
void TForm2::UpdateProgress(int AValue, int ASpeed, bool &Abort)
{
	Abort = !BStopDownload->Enabled;
	ProgressBarDownload->Value = AValue;
	TVarRec LVarRec[1];
	LVarRec[0] = ASpeed;
	LabelGlobalSpeed->Text = Format("Global speed: %d KB/s", LVarRec, 0);
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ReceiveDataEvent(TObject * const Sender, __int64 AContentLength, __int64 AReadCount, bool &Abort)
{
	if (Abort == false) {
		UInt32 LTime = TThread::GetTickCount() - FGlobalStart;
		int LSpeed = (AReadCount * 1000) / LTime;
		bool LAbort = Abort;
		System::Classes::TThread::Synchronize(NULL, SyncLambda<int, int, bool&>(&this->UpdateProgress, AReadCount, LSpeed, LAbort));
		Abort = LAbort;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormCreate(TObject *Sender)
{
	FClient = THTTPClient::Create();
	FClient->OnReceiveData = ReceiveDataEvent;
}
//---------------------------------------------------------------------------
void __fastcall TForm2::FormDestroy(TObject *Sender)
{
    FClient->Free();
}
//---------------------------------------------------------------------------
