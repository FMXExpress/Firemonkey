//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop
#include <memory>
#include "NotesClientModuleU.h"
#include <FMX.Dialogs.hpp>
#include <REST.Types.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"
TNotesClientModule *NotesClientModule;
//---------------------------------------------------------------------------
__fastcall TNotesClientModule::TNotesClientModule(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
TJSONArray * __fastcall TNotesClientModule::GetJSONArray(const TCustomRESTResponse *AResponse)
{
	if(const_cast<TCustomRESTResponse*>(AResponse)->JSONValue == NULL) {
		throw new System::Sysutils::Exception("JSON expected");
	}
	return static_cast<TJSONArray*>(const_cast<TCustomRESTResponse*>(AResponse)->JSONValue);
}
//---------------------------------------------------------------------------
TJSONObject * __fastcall TNotesClientModule::GetJSONObject(const TCustomRESTResponse *AResponse)
{
	if(const_cast<TCustomRESTResponse*>(AResponse)->JSONValue == NULL) {
		throw new System::Sysutils::Exception("JSON expected");
	}
	return static_cast<TJSONObject*>(const_cast<TCustomRESTResponse*>(AResponse)->JSONValue);
}
//---------------------------------------------------------------------------
bool __fastcall TNotesClientModule::GetLoggedIn(void)
{
	return BackendAuth1->LoggedIn;
}
//---------------------------------------------------------------------------
String __fastcall TNotesClientModule::GetLoggedInUserName(void)
{
	return BackendAuth1->LoggedInUserName;
}
 //---------------------------------------------------------------------------
void __fastcall TNotesClientModule::Login(const String & AUserName, const String & APassword)
{
	BackendAuth1->UserName = AUserName;
	BackendAuth1->Password = APassword;
	BackendAuth1->Login();
}
//---------------------------------------------------------------------------
void __fastcall TNotesClientModule::Logout(void)
{
	BackendAuth1->Logout();
}
//---------------------------------------------------------------------------
void __fastcall TNotesClientModule::Signup(const String & AUserName, const String & APassword)
{
	BackendAuth1->UserName = AUserName;
	BackendAuth1->Password = APassword;
	BackendAuth1->Signup();
}
//---------------------------------------------------------------------------
void __fastcall TNotesClientModule::AddNote(const TNote * ANote, String &AID)
{
	TBackendEndpoint * lEndpoint = BackendEndpointAddNote;
	std::auto_ptr<TJSONObject> ljsonObj(TNoteJSON::NoteToJSON(ANote));
	lEndpoint->ClearBody();
	lEndpoint->AddBody(ljsonObj.get());
	lEndpoint->Execute();
	AID = GetJSONObject(lEndpoint->Response)->Get("id")->JsonValue->Value();
}
//---------------------------------------------------------------------------
bool __fastcall TNotesClientModule::FindNote(const String & ATitle, TNote * ANote)
{
	TBackendEndpoint *lEndpoint = BackendEndpointGetNotes;
	TRESTRequestParameter * lParam = lEndpoint->Params->AddItem();
	std::vector<TNote*> lNotes;
	bool _return = false;
	try
	{
		// Add a parameter to get note request
		lParam->Kind = TRESTRequestParameterKind::pkGETorPOST;
		lParam->name = "title";
		lParam->Value = ATitle;
		BackendEndpointGetNote->Execute();
		lNotes = TNoteJSON::JSONToNotes(GetJSONArray(lEndpoint->Response));
		if(lNotes.size() == 0) {
			_return = false;
		}
		std::vector<TNote*>::iterator it = lNotes.begin();
		ANote = *it;
		_return = true;
    }
     __finally
	{
		lEndpoint->Params->Delete(lParam);
	}
	return _return;
}
//---------------------------------------------------------------------------
bool __fastcall TNotesClientModule::DeleteNote(const String &AID)
{
	TBackendEndpoint *lEndpoint = BackendEndpointDeleteNote;
	lEndpoint->Params->Items[0]->Value = AID;
	lEndpoint->AllowHTTPErrors = TBackendEndpoint::TAllowHTTPErrors::ClientErrorNotFound_404;
	lEndpoint->Execute();
	return lEndpoint->Response->Status.Success();
}
//---------------------------------------------------------------------------
bool __fastcall TNotesClientModule::GetNote(const String &AID, TNote *ANote)
{
	bool _return = false;
	TBackendEndpoint *lEndpoint = BackendEndpointGetNote;
	lEndpoint->Params->Items[0]->Value = AID;
	lEndpoint->AllowHTTPErrors = TBackendEndpoint::TAllowHTTPErrors::ClientErrorNotFound_404;
	lEndpoint->Execute();
	_return = lEndpoint->Response->Status.Success();
	if(_return) {
		ANote = TNoteJSON::JSONToNote(GetJSONObject(lEndpoint->Response));
	}
	return _return;
}
//---------------------------------------------------------------------------
void __fastcall TNotesClientModule::UpdateNote(const TNote * ANote)
{
	TBackendEndpoint * lEndpoint = BackendEndpointUpdateNote;
	lEndpoint->Params->Items[0]->Value = ANote->ID;
	std::auto_ptr<TJSONObject> ljsonObj(TNoteJSON::NoteToJSON(ANote));
	lEndpoint->ClearBody();
	lEndpoint->AddBody(ljsonObj.get());
	lEndpoint->Execute();
}
//---------------------------------------------------------------------------
std::vector<TNote*> __fastcall TNotesClientModule::GetNotes(void)
{
	TBackendEndpoint * lEndpoint = BackendEndpointGetNotes;
	lEndpoint->Execute();
	return TNoteJSON::JSONToNotes(GetJSONArray(lEndpoint->Response));
}
//---------------------------------------------------------------------------
