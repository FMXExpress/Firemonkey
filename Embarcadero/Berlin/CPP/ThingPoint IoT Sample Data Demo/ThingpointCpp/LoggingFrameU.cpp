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
#include <EMSHosting.LoggingService.hpp>
#include <System.StrUtils.hpp>

#pragma hdrstop
#include <memory>

#include "LoggingFrameU.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TEMSEdgeLoggingFrame * FFrame;
// ---------------------------------------------------------------------------

struct TThreadHandler : public TCppInterfacedObject<TThreadProcedure> {
    String FCategory;
    TJSONObject * FJSON;

    __fastcall TThreadHandler::TThreadHandler(String ACategory,
	TJSONObject * AJSON) {
	FCategory = ACategory;
	FJSON = AJSON;
    }

    void __fastcall Invoke(void) {
	if (FFrame != NULL)
	    FFrame->LogItem(FCategory, FJSON);
	else
	    delete FJSON;
    }
};

struct TLoggingHandler
    : public TCppInterfacedObject
    <Emshosting::Loggingservice::TEMSLoggingService::TLogProc> {
    void __fastcall Invoke(const System::UnicodeString ACategory,
	System::Json::TJSONObject* const AJSON) {
	TJSONObject * LJSON = dynamic_cast<TJSONObject*>(AJSON->Clone());
	_di_TThreadProcedure handler = new TThreadHandler(ACategory, LJSON);
	TThread::Queue(NULL, handler);
    }
};

__fastcall TEMSEdgeLoggingFrame::TEMSEdgeLoggingFrame(TComponent* Owner)
    : TFrame(Owner) {
    if (FFrame != NULL) {
	throw new Exception("Only one logging frame allowed in an application");
    }
    FFrame = this;
    Emshosting::Loggingservice::TEMSLoggingService::OnLog = NULL;

    Emshosting::Loggingservice::TEMSLoggingService::_di_TLogProc handler =
	new TLoggingHandler();
    Emshosting::Loggingservice::TEMSLoggingService::OnLog = handler;
}

// ---------------------------------------------------------------------------
void __fastcall TEMSEdgeLoggingFrame::ButtonClearClick(TObject *Sender) {
    Memo1->Lines->Clear();
}
// ---------------------------------------------------------------------------

void __fastcall TEMSEdgeLoggingFrame::LogItem(String ACategory,
    TJSONObject * AJSON) {
    std::auto_ptr<TJSONObject>LJSON(new TJSONObject());
    LJSON->AddPair(ACategory, AJSON);
    // Note AJSON is owned by LJSON so AJSON will be freed
    String LLine = LJSON->ToString();
    // Remove line breaks
    LLine = System::Strutils::ReplaceStr(LLine, "\r\n", ", ");
    Memo1->Lines->Add(LLine);
}
