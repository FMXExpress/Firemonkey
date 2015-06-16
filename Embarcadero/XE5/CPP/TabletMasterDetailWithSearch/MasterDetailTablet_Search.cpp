// ---------------------------------------------------------------------------

#include <fmx.h>
#include <cwchar>
#pragma hdrstop

#include "MasterDetailTablet_Search.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
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
		!(reinterpret_cast<IScopeNavigator*>(PrototypeBindSource1))->BOF && !Filtered();
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
		!(reinterpret_cast<IScopeNavigator*>(PrototypeBindSource1))->Eof && !Filtered();
}

// ---------------------------------------------------------------------------
bool __fastcall TTabletSearchForm::Filtered() {
	return ListView1->Items->Filter;
}

// ---------------------------------------------------------------------------
void __fastcall TTabletSearchForm::Edit1ChangeTracking(TObject *Sender)
{
	Lower = LowerCase(Trim(Edit1->Text));
	if (Lower == "") {
		if (Filtered()) {
			// Clear filter
			ListView1->Items->Filter = NULL;
			ListView1->ItemIndex = PrototypeBindSource1->ItemIndex;
		}
	}
	else {
		// Start or update filter
		ListView1->ItemIndex = -1;
		ListView1->Items->Filter = new TMyPredicate(Lower);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TTabletSearchForm::ListView1ItemClick(const TObject *Sender,
	const TListViewItem *AItem)
{
	// Goto a row in the data source
	PrototypeBindSource1->Locate("ContactName1", TValue::_op_Implicit(AItem->Text));
}
// ---------------------------------------------------------------------------
