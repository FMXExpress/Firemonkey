//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "BottomDetailMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm594 *Form594;

const int cItemHeight = 80;
const int cTextHeight = 70;
const int cDetailHeight = 30;
const int cGlyphWidth = 30;
const int cImageSize = 70;
const int cTextOffset = cImageSize + 5;
const int cEditImageOffset = cGlyphWidth;
const int cEditTextOffset = cGlyphWidth + cImageSize + 5;
//---------------------------------------------------------------------------
__fastcall TForm594::TForm594(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SetTextProperties( TTextObjectAppearance *AText)
{
	AText->RestoreDefaults(); // Restore to defaults for custom
	AText->Height = cTextHeight;
	AText->VertAlign = TListItemAlign::Trailing;
	AText->TextVertAlign = TTextAlign::Leading;
	AText->PlaceOffset->X = cTextOffset;
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SetDetailProperties( TTextObjectAppearance *ADetail)
{
	ADetail->RestoreDefaults(); // Restore to defaults for custom
	ADetail->Height = cDetailHeight;
	ADetail->VertAlign = TListItemAlign::Trailing;
	ADetail->TextVertAlign = TTextAlign::Leading;
	ADetail->PlaceOffset->X = cTextOffset;
	ADetail->Visible = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SetImageProperties( TImageObjectAppearance * AImage)
{
	AImage->RestoreDefaults(); // Restore to defaults for custom
	AImage->Height = cImageSize;
	AImage->Width = cImageSize;
	AImage->VertAlign = TListItemAlign::Center;
	AImage->Visible = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SetCommonProperties( TItemAppearanceObjects *AObjects)
{
	SetTextProperties(AObjects->Detail);
	SetDetailProperties(AObjects->Detail);
	SetImageProperties(AObjects->Image);
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SetEditItemProperties( TItemAppearanceObjects * AObjects)
{
	AObjects->Image->PlaceOffset->X = cEditImageOffset;
	AObjects->Text->PlaceOffset->X = cEditTextOffset;
	AObjects->Detail->PlaceOffset->X = cEditTextOffset;
	AObjects->GlyphButton->Visible = true;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// Set appearance properties in code.  This is an alternative to setting properties in the object inspector.
// Clicking the button will have no affect, if the design time properties match.
void __fastcall TForm594::SpeedButtonSetPropsClick(TObject *Sender)
{
	TItemAppearanceObjects * LObjects = NULL;
	ListViewBottomDetail->BeginUpdate();
	try {
		ListViewBottomDetail->ItemAppearance->ItemHeight = cItemHeight;
		ListViewBottomDetail->ItemAppearance->ItemEditHeight = cItemHeight;
		// Set Item properties
		LObjects = reinterpret_cast<TItemAppearanceObjects*>(ListViewBottomDetail->ItemAppearanceObjects->ItemObjects);
		LObjects->BeginUpdate();
		try {
			SetCommonProperties(LObjects);
		}
		__finally {
			LObjects->EndUpdate();
		}
		// Set Edit Item properties
		LObjects = reinterpret_cast<TItemAppearanceObjects*>(ListViewBottomDetail->ItemAppearanceObjects->ItemEditObjects);
        LObjects->BeginUpdate();
		try {
			SetCommonProperties(LObjects);
			SetEditItemProperties(LObjects);
		}
		__finally {
			LObjects->EndUpdate();
		}
	}
	__finally {
		ListViewBottomDetail->EndUpdate();
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm594::ToggleEditModeClick(TObject *Sender)
{
	ListViewBottomDetail->EditMode = !ListViewBottomDetail->EditMode;
}
//---------------------------------------------------------------------------

void __fastcall TForm594::SpeedButtonFillClick(TObject *Sender)
{
	LinkFillControlToField1->Active = false;
	// Code to fill TListView
	for (int i = 1; i <= 20; i++) {
		TListViewItem * item = ListViewBottomDetail->Items->Add();
		item->Text = Format("Text %d", ARRAYOFCONST((i)));
		item->Detail = Format("Detail %d", ARRAYOFCONST((i)));
		item->BitmapRef = ImageRAD->Bitmap;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm594::SpeedButtonLiveBindingsClick(TObject *Sender)
{
	LinkFillControlToField1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm594::ListViewBottomDetailUpdateObjects(const TObject *Sender,
		  const TListViewItem *AItem)
{
	TCustomListView * parent = const_cast<TListViewItem*>(AItem)->Parent;
	int outerWidth = parent->Width - parent->ItemSpaces->Left - parent->ItemSpaces->Right - 8;
	if (const_cast<TListViewItem*>(AItem)->Objects->AccessoryObject->Visible) {
		outerWidth -= const_cast<TListViewItem*>(AItem)->Objects->AccessoryObject->Width;
	}
	// Adjust elements to width of listview so that text truncation works
	const_cast<TListViewItem*>(AItem)->Objects->TextObject->Width =
		Max(1, static_cast<int>(outerWidth - const_cast<TListViewItem*>(AItem)->Objects->TextObject->PlaceOffset->X));
	const_cast<TListViewItem*>(AItem)->Objects->DetailObject->Width =
		const_cast<TListViewItem*>(AItem)->Objects->TextObject->Width;
}
//---------------------------------------------------------------------------
