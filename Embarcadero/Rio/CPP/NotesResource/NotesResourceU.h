//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotesResourceUH
#define NotesResourceUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <EMS.Services.hpp>
#include <EMS.ResourceAPI.hpp>
#include <EMS.ResourceTypes.hpp>
#include "NotesStorageU.h"
#include "NoteTypesU.h"
//---------------------------------------------------------------------------
#pragma explicit_rtti methods (public)
class TNotesResource1 : public TDataModule
{
__published:
private:
	TNotesStorage * FNotesStorage;
public:
	__fastcall TNotesResource1(TComponent* Owner);
	__fastcall ~TNotesResource1(void);
	void Get(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
	void GetItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
	void Post(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
	void PutItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
	void DeleteItem(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
private:
	void CheckNotesManager(const TEndpointContext * AContext);
	void HandleException(void);
};
#endif


