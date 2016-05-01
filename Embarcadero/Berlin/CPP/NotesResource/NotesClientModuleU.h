//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotesClientModuleUH
#define NotesClientModuleUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.BindSource.hpp>
#include <REST.Backend.EMSProvider.hpp>
#include <REST.Backend.EMSServices.hpp>
#include <REST.Backend.EndPoint.hpp>
#include <REST.Backend.MetaTypes.hpp>
#include <REST.Backend.ServiceComponents.hpp>
#include <REST.Backend.ServiceTypes.hpp>
#include <REST.Client.hpp>
#include <System.JSON.hpp>
#include "NoteTypesU.h"
#include <vector>
//---------------------------------------------------------------------------
class TNotesClientModule : public TDataModule
{
__published:	// IDE-managed Components
	TBackendAuth *BackendAuth1;
	TBackendEndpoint *BackendEndpointDeleteNote;
	TBackendEndpoint *BackendEndpointUpdateNote;
	TBackendEndpoint *BackendEndpointGetNotes;
	TEMSProvider *EMSProvider1;
	TBackendEndpoint *BackendEndpointGetNote;
	TBackendEndpoint *BackendEndpointAddNote;
private:	// User declarations
	TJSONArray * __fastcall GetJSONArray(const TCustomRESTResponse *AResponse);
	TJSONObject * __fastcall GetJSONObject(const TCustomRESTResponse *AResponse);
	bool __fastcall GetLoggedIn(void);
	String __fastcall GetLoggedInUserName(void);
public:		// User declarations
	__fastcall TNotesClientModule(TComponent* Owner);
	void __fastcall Login(const String & AUserName, const String & APassword);
	void __fastcall Logout(void);
	void __fastcall Signup(const String & AUserName, const String & APassword);
	void __fastcall AddNote(const TNote * ANote, String &AID);
	bool __fastcall FindNote(const String & ATitle, TNote * ANote);
	bool __fastcall DeleteNote(const String &AID);
	bool __fastcall GetNote(const String &AID, TNote *ANote);
	void __fastcall UpdateNote(const TNote * ANote);
    std::vector<TNote*> __fastcall GetNotes(void);
	__property bool LoggedIn = {read=GetLoggedIn};
	__property String LoggedInUserName = {read=GetLoggedInUserName};
};
//---------------------------------------------------------------------------
extern PACKAGE TNotesClientModule *NotesClientModule;
//---------------------------------------------------------------------------
#endif
