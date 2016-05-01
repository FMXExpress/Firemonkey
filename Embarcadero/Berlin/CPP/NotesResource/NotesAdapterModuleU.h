//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotesAdapterModuleUH
#define NotesAdapterModuleUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include "NoteTypesU.h"
//---------------------------------------------------------------------------

#pragma explicit_rtti methods () properties (public) fields()
class TNoteWrapper : public TObject {
private:
	std::auto_ptr<TNote> FNote;
	inline String __fastcall GetContent(void){ return FNote->Content;}
	inline String __fastcall GetID(void) { return FNote->ID;}
	inline String __fastcall GetTitle(void) { return FNote->Title;}
	inline void __fastcall SetContent(String AValue) { FNote->Content = AValue;}
	inline void __fastcall SetTitle(String AValue) { FNote->Title = AValue;}
public:
	__fastcall TNoteWrapper(TNote * ANote) : FNote(ANote){ }
 	virtual void __fastcall AfterConstruction(void) { if (FNote.get() == NULL) FNote.reset(new TNote()); }
	inline TNote * __fastcall GetNote(void) { return FNote.get();}
	inline void __fastcall SetID(const String & AID) { FNote->ID = AID;}
	__property String Title = {read=GetTitle, write=SetTitle};
	__property String Content = {read=GetContent, write=SetContent};
	__property String ID = {read=GetID};
};

#pragma explicit_rtti methods () properties () fields()
class TNotesAdapterModule : public TDataModule
{
__published:	// IDE-managed Components
private:	// User declarations
	TListBindSourceAdapter * FBindSourceAdapter;
	void __fastcall AfterPost(TBindSourceAdapter * Sender);
	void __fastcall BeforeDelete(TBindSourceAdapter * Sender);
public:		// User declarations
	__fastcall TNotesAdapterModule(TComponent* Owner);
	TBindSourceAdapter* __fastcall GetBindSourceAdapter(void);
	void __fastcall UpdateAdapter(std::vector<TNote*> *ANotes);
	void __fastcall RefreshAdapter(void);
	__property TBindSourceAdapter *BindSourceAdapter = {read=GetBindSourceAdapter};
};
//---------------------------------------------------------------------------
extern PACKAGE TNotesAdapterModule *NotesAdapterModule;
//---------------------------------------------------------------------------
#endif
