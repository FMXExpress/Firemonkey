//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "customlistfrm.h"
#include "DelphiIntf.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

long rndm(long max) {
#if  defined __APPLE__ || defined __ANDROID__
	return (random() % ((max + 1) + max));
#else
	return random(max);
#endif
}

TfrmCustomList *frmCustomList;

// ---------------------------------------------------------------------------
TFmxObject *FindItemParent(TFmxObject* Obj, String ParentClass) {
	TFmxObject* result;
	if (Obj->Parent != NULL) {
		if (SameText(Obj->Parent->ClassName(), ParentClass)) {
			return Obj->Parent;
		} else {
			return FindItemParent(Obj->Parent, ParentClass);
		}
	}
	return NULL;
}
// ---------------------------------------------------------------------------
__fastcall TfrmCustomList::TfrmCustomList(TComponent* Owner) : TForm(Owner) {
	OpenDialog1->Filter = TBitmapCodecManager::GetFilterString();
}

// ---------------------------------------------------------------------------
void __fastcall TfrmCustomList::DoInfoClick(TObject *Sender) {
	TListBoxItem * Item = (TListBoxItem *) FindItemParent((TFmxObject *)Sender, "TListBoxItem");
	InfoLabel->Text = "Info Button click on " + IntToStr(Item->Index) +
		" listbox item";
}
// ---------------------------------------------------------------------------
void __fastcall TfrmCustomList::DoVisibleChange(TObject *Sender) {
	TListBoxItem * Item = (TListBoxItem *) FindItemParent((TFmxObject *)Sender, "TListBoxItem");
	InfoLabel->Text = "Checkbox changed " + IntToStr(ListBox1->ItemIndex) +
		" listbox item to " + BoolToStr
		(Item->StylesData["visible"].AsBoolean(), true);
}

// ---------------------------------------------------------------------------
void __fastcall TfrmCustomList::Button1Click(TObject *Sender) {
	TListBoxItem *Item;
	int i;

	if (OpenDialog1->Execute()) {
		for (i = 0; i < OpenDialog1->Files->Count; i++) {
			String currentFile = OpenDialog1->Files->operator [](i);
			Item = new TListBoxItem(NULL);
			Item->Parent = ListBox1;
			Item->StyleLookup = "CustomItem";
			Item->TagString = currentFile;
			Item->Text = currentFile;
			Item->StylesData["icon"] = TValue::From<String>(currentFile);
			Item->StylesData["resolution"] = TValue::From<String>("1024x768 px"); // set size
			Item->StylesData["depth"] = TValue::From<String>("32 bit");
			Item->StylesData["visible"] = TValue::From<bool>(true); // set Checkbox value
			Item->StylesData["visible.OnChange"] = NotifyEventAsTValue(DoVisibleChange); // set OnChange value
			Item->StylesData["info.OnClick"] = NotifyEventAsTValue(DoInfoClick); // set OnClick value
		}
		Caption = IntToStr(ListBox1->Count) + " items";
	};
}

// ---------------------------------------------------------------------------
void __fastcall TfrmCustomList::Button2Click(TObject *Sender) {
	TListBoxItem *Item;

	Item = new TListBoxItem(NULL);
	Item->Parent = ListBox1;
	Item->StyleLookup = "CustomItem";
	Item->Text = "item " + IntToStr(Item->Index);
	if (Item->Index % 2) {
		Item->ItemData->Bitmap = Image1->Bitmap;
	} else {
		Item->ItemData->Bitmap = Image2->Bitmap;
	}
	Item->StylesData["resolution"] = TValue::From<String>("1024x768 px"); // set size
	Item->StylesData["depth"] = TValue::From<String>("32 bit");
	Item->StylesData["visible"] = TValue::From<bool>(true); // set Checkbox value
	Item->StylesData["visible.OnChange"] = NotifyEventAsTValue(DoVisibleChange); // set OnChange value
	Item->StylesData["info.OnClick"] = NotifyEventAsTValue(DoInfoClick); // set OnClick value
   	Caption = IntToStr(ListBox1->Count) + " items";
}
// ---------------------------------------------------------------------------

void __fastcall TfrmCustomList::CheckBox1Change(TObject *Sender) {
	ListBox1->AllowDrag = CheckBox1->IsChecked;
}
// ---------------------------------------------------------------------------

void __fastcall TfrmCustomList::Button3Click(TObject *Sender) {
	int i;
	ListBox1->BeginUpdate();
	for (i = 1; i < 1000; i++)
		Button2Click(Sender);
	ListBox1->EndUpdate();
}
// ---------------------------------------------------------------------------
