//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "NotesResourceU.h"
#include <memory>
#include <vector>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "System.Classes.TPersistent"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TNotesResource1::TNotesResource1(TComponent* Owner)
	: TDataModule(Owner)
{
}

__fastcall TNotesResource1::~TNotesResource1(void)
{
    FreeAndNil(FNotesStorage);
}

void TNotesResource1::HandleException(void)
{
	String LMessage = "";
	TObject * lException = ExceptObject();
	if(lException != NULL) {
		if(static_cast<Exception*>(lException)) {
			LMessage = static_cast<Exception*>(lException)->Message;
			if(static_cast<ENoteDuplicate*>(lException)) {
				EEMSHTTPError::RaiseDuplicate(LMessage);
			}
			else if(static_cast<ENoteNotFound*>(lException)) {
                EEMSHTTPError::RaiseNotFound(LMessage);
			}
			else if(static_cast<ENoteMissingTitle*>(lException)) {
                EEMSHTTPError::RaiseBadRequest(LMessage);
			}
			else {
				lException = (TObject*)AcquireExceptionObject();
				if(lException != NULL) {
					throw lException;
				}
			}
        }
    }
}

void TNotesResource1::Get(TEndpointContext* AContext, TEndpointRequest* ARequest,
	TEndpointResponse* AResponse)
{
	String LTitle = "";
	std::auto_ptr<TNote> LNote(new TNote());
	std::vector<TNote*> * lNotes = NULL;
	TJSONArray * lJson = NULL;
	try {
		this->CheckNotesManager(AContext);
		if(ARequest->Params->TryGetValue("title", LTitle)) {
			// Find a note with a particular title
			if(FNotesStorage->FindNote(LTitle, LNote.get())) {
				lNotes = new std::vector<TNote*>();
				lNotes->push_back(LNote.get());
			}
			else {
				lNotes = NULL;
			}
		}
		else {
			lNotes = FNotesStorage->GetNotes();
		}
		lJson = TNoteJSON::NotesToJSON(lNotes);
		AResponse->Body->SetValue(lJson, true);
	}
	catch(...) {
		FreeAndNil(lJson);
		HandleException();
	}
}

void TNotesResource1::GetItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		String lItem = ARequest->Params->Values["item"];
		std::auto_ptr<TNote> lNote(new TNote());
		CheckNotesManager(AContext);
		if(FNotesStorage->GetNote(lItem, lNote.get())) {
			TJSONObject * jsonObj = TNoteJSON::NoteToJSON(lNote.get());
			AResponse->Body->SetValue(jsonObj, true);
		} else {
			AResponse->RaiseNotFound(String().sprintf(L"\"%s\" not found", lItem.c_str()));
		}
	} catch (...) {
		HandleException();
	}
}

void TNotesResource1::Post(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		TJSONObject * lJson = NULL;
		if(ARequest->Body->TryGetObject(lJson)) {
			CheckNotesManager(AContext);
			TNote * lNote = TNoteJSON::JSONToNote(lJson);
			String lId = "";
			FNotesStorage->AddNote(*lNote, lId);
			lJson = new TJSONObject();
			lJson->AddPair(TNoteJSON::TNames::Id, lId);
			AResponse->Body->SetValue(lJson, true);
		}
		else {
			AResponse->RaiseBadRequest("JSON expected");
		}
	} catch (...) {
		HandleException();
	}
}

void TNotesResource1::PutItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try {
		String lItem = ARequest->Params->Values["item"];
		TJSONObject * lJson = NULL;
		if(ARequest->Body->TryGetObject(lJson)) {
			CheckNotesManager(AContext);
			TNote * lNote = TNoteJSON::JSONToNote(lJson);
			FNotesStorage->UpdateNote(lItem, lNote);
		}
		else {
		   AResponse->RaiseBadRequest("JSON expected");
        }
	} catch (...) {
		HandleException();
	}
}

String GetModuleDirectory(void)
{
	return ExtractFilePath(StringReplace(GetModuleName((unsigned int)HInstance), "\\\\?\\",
		"", TReplaceFlags() << System::Sysutils::rfReplaceAll));
}

void TNotesResource1::CheckNotesManager(const TEndpointContext * AContext)
{
	if(const_cast<TEndpointContext*>(AContext)->User == NULL) {
		const_cast<TEndpointContext*>(AContext)->Response->RaiseUnauthorized("The operation is only permitted for logged in users");
	}
	if(FNotesStorage == NULL) {
		FNotesStorage = new TNotesStorage(GetModuleDirectory(), const_cast<TEndpointContext*>(AContext)->User->UserID);
    }
}

void TNotesResource1::DeleteItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
	try{
		String lItem = ARequest->Params->Values["item"];
		CheckNotesManager(AContext);
		FNotesStorage->DeleteNote(lItem);
	}catch(...) {
		HandleException();
    }
}

namespace Notesresourceu
{
	void __fastcall PACKAGE Register()
	{
        	std::auto_ptr<TEMSResourceAttributes> attributes(new TEMSResourceAttributes());
                attributes->ResourceName = "Notes";
                attributes->ResourceSuffix["GetItem"] = "{item}";
                attributes->ResourceSuffix["PutItem"] = "{item}";
                attributes->ResourceSuffix["DeleteItem"] = "{item}";
  		RegisterResource(__typeinfo(TNotesResource1), attributes.release());
	}
}


