//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <algorithm>
#include <list>
#pragma hdrstop

#include "ListViewCheckListForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm2 *Form2;

std::list<int> FChecked;

//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem)

{
	// Toggle visibility of accessory when item is clicked
	// Save checked state of item
	int _index = (const_cast<TListViewItem*>(AItem))->Index;
	if ((const_cast<TListViewItem*>(AItem))->Objects->AccessoryObject->Visible)
	{
		(const_cast<TListViewItem*>(AItem))->Objects->AccessoryObject->Visible = false;
		FChecked.remove(_index);
	}
	else
	{
		(const_cast<TListViewItem*>(AItem))->Objects->AccessoryObject->Visible = true;
		FChecked.push_back(_index);
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm2::ListView1UpdateObjects(const TObject *Sender, const TListViewItem *AItem)
{
	// In order for text to be truncated properly, shorten text object
	TListViewItem::TListViewItemObjects * _objects = (const_cast<TListViewItem*>(AItem))->Objects;
	_objects->TextObject->Width = _objects->TextObject->Width - (5 + _objects->AccessoryObject->Width);
	// Restore checked state when device is rotated.
	// When listview is resized because of rotation, accessory properties will be reset to default values
	if(!FChecked.empty()){
		std::list<int>::iterator it = std::find(FChecked.begin(), FChecked.end(), (const_cast<TListViewItem*>(AItem))->Index);
		if(it != FChecked.end()) {
			_objects->AccessoryObject->Visible = true;
		} else {
            _objects->AccessoryObject->Visible = false;
        }
	}
}
//---------------------------------------------------------------------------
