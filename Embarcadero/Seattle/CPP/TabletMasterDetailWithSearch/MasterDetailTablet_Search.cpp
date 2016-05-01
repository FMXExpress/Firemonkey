//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <cwchar>
#pragma hdrstop

#include "MasterDetailTablet_Search.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)
#pragma resource ("*.LgXhdpiTb.fmx", _PLAT_ANDROID)

TTabletSearchForm *TabletSearchForm;

// ---------------------------------------------------------------------------
__fastcall TTabletSearchForm::TTabletSearchForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TTabletSearchForm::LiveBindingsBindNavigatePrior1Execute
	(TObject *Sender)

{
	PrototypeBindSource1->Prior();
	if (!Filtered())
		ListView1->ItemIndex = PrototypeBindSource1->ItemIndex;

}
// ---------------------------------------------------------------------------

void __fastcall TTabletSearchForm::LiveBindingsBindNavigatePrior1Update(TObject *Sender)
{
	dynamic_cast<TAction*>(Sender)->Enabled =
		!PrototypeBindSource1->Eof && !Filtered();
}
// ---------------------------------------------------------------------------

void __fastcall TTabletSearchForm::LiveBindingsBindNavigateNext1Execute(TObject *Sender)
{
	PrototypeBindSource1->Next();
	if (!Filtered())
		ListView1->ItemIndex = PrototypeBindSource1->ItemIndex;
}
// ---------------------------------------------------------------------------

void __fastcall TTabletSearchForm::LiveBindingsBindNavigateNext1Update(TObject *Sender)
{
	dynamic_cast<TAction*>(Sender)->Enabled =
		!PrototypeBindSource1->Eof && !Filtered();
}

// ---------------------------------------------------------------------------
bool __fastcall TTabletSearchForm::Filtered() {
	return ListView1->Items->Filtered;
}

// ---------------------------------------------------------------------------
void __fastcall TTabletSearchForm::ListView1ItemClick(const TObject *Sender,
	const TListViewItem *AItem)
{
	// Goto a row in the data source
	PrototypeBindSource1->Locate("ContactName1", TValue::_op_Implicit(AItem->Text));
}
// ---------------------------------------------------------------------------
