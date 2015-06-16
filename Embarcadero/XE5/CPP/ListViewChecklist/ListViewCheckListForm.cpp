//---------------------------------------------------------------------------

#include <fmx.h>
#include <algorithm>
#pragma hdrstop

#include "ListViewCheckListForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm2 *Form2;
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
		std::list<int>::iterator it = std::find(FChecked.begin(), FChecked.end(),
			(const_cast<TListViewItem*>(AItem))->Index);
		_objects->AccessoryObject->Visible = *it;
	}
}
//---------------------------------------------------------------------------


