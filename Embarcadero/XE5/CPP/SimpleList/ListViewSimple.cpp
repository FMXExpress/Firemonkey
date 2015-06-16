// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "ListViewSimple.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TSimpleListViewDelete *SimpleListViewDelete;

// ---------------------------------------------------------------------------
__fastcall TSimpleListViewDelete::TSimpleListViewDelete(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TSimpleListViewDelete::DeleteButtonClick(TObject *Sender) {
	ListView1->BeginUpdate();
	DynamicArray<int> selected = ListView1->Items->CheckedIndexes(true);
	for(int i = 0; i <	selected.Length ; i++)
	{
		ListView1->Items->Delete(selected[i]);
	}
	ListView1->EndUpdate();
}
// ---------------------------------------------------------------------------

void __fastcall TSimpleListViewDelete::DoneButtonClick(TObject *Sender) {
	ListView1->EditMode = false;
	DoneButton->Visible = false;
	DeleteButton->Visible = false;
	EditButton->Visible = true;
}
// ---------------------------------------------------------------------------

void __fastcall TSimpleListViewDelete::EditButtonClick(TObject *Sender) {
	ListView1->EditMode = true;
	DoneButton->Visible = true;
	DeleteButton->Visible = true;
	EditButton->Visible = false;
}
// ---------------------------------------------------------------------------
