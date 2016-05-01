//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "NotesAdapterModuleU.h"
#include "NotesClientModuleU.h"
#include <FMX.Dialogs.hpp>
#include <REST.Types.hpp>
#include <vector>
#include <memory>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"

TNotesAdapterModule *NotesAdapterModule;
//---------------------------------------------------------------------------
__fastcall TNotesAdapterModule::TNotesAdapterModule(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TNotesAdapterModule::AfterPost(TBindSourceAdapter * Sender)
{
	TNoteWrapper * lWrapper = (TNoteWrapper*)FBindSourceAdapter->List->Items[FBindSourceAdapter->ItemIndex];
	try
	{
		if(lWrapper->ID.IsEmpty()) {
			String lID = "";
			NotesClientModule->AddNote(lWrapper->GetNote(), lID);
			lWrapper->SetID(lID);
		} else {
			NotesClientModule->UpdateNote(lWrapper->GetNote());
		}
	} catch(...) {
		FBindSourceAdapter->Edit();
		throw;
	}
}
//---------------------------------------------------------------------------
void __fastcall TNotesAdapterModule::BeforeDelete(TBindSourceAdapter * Sender)
{
	TNoteWrapper * lWrapper = (TNoteWrapper*)FBindSourceAdapter->List->Items[FBindSourceAdapter->ItemIndex];
	if(!lWrapper->ID.IsEmpty()) {
        	NotesClientModule->DeleteNote(lWrapper->ID);
    }
}
//---------------------------------------------------------------------------
TBindSourceAdapter* __fastcall TNotesAdapterModule::GetBindSourceAdapter(void)
{
	if(FBindSourceAdapter == NULL) {
		TList__1<TObject*> * LList = new TList__1<TObject*>();
		FBindSourceAdapter = new TListBindSourceAdapter(this, LList, __classid(TNoteWrapper), true);
		FBindSourceAdapter->AfterPost = &AfterPost;
		FBindSourceAdapter->BeforeDelete = &BeforeDelete;
	}
	return FBindSourceAdapter;
}
//---------------------------------------------------------------------------
void __fastcall TNotesAdapterModule::UpdateAdapter(std::vector<TNote*> *ANotes)
{
	if (ComponentState.Contains(csDestroying)) {
		return;
	}
	TList__1<TObject*> *lList = new TList__1<TObject*>();
	try {
		for(std::vector<TNote*>::iterator it = ANotes->begin(); it != ANotes->end(); it++)
		{
			lList->Add(new TNoteWrapper(*it));
                }
                if(FBindSourceAdapter == NULL) {
                        FBindSourceAdapter = new TListBindSourceAdapter(this, lList, __classid(TNoteWrapper), true);
                }
                else {
                	FBindSourceAdapter->SetList(lList, true);
                }
                FBindSourceAdapter->Active = true;
	} catch (...) {
		FreeAndNil(lList);
		throw;
	}
}
//---------------------------------------------------------------------------
void __fastcall TNotesAdapterModule::RefreshAdapter(void)
{
	std::vector<TNote*> lNotes = NotesClientModule->GetNotes();
	UpdateAdapter(&lNotes);
}
//---------------------------------------------------------------------------
