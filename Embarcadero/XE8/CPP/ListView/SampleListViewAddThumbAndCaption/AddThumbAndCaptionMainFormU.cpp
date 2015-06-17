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

#include "AddThumbAndCaptionMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

const System::UnicodeString sThumbNailName = L"TI";
const System::UnicodeString sCaption = L"CA";

TForm594 *Form594;
//---------------------------------------------------------------------------
__fastcall TForm594::TForm594(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm594::ListViewBottomDetailUpdateObjects(const TObject *Sender,
		  const TListViewItem *AItem)
{
	TListItemImage * LImage = reinterpret_cast<TListItemImage*>((const_cast<TListViewItem*>(AItem))->Objects->FindObject(sThumbNailName));
	if (LImage == NULL)
	{
		LImage = new TListItemImage((TListItem*)AItem);
		LImage->Name = sThumbNailName;
		LImage->Align = TListItemAlign::Trailing;
		LImage->PlaceOffset->Y = 5;
		LImage->PlaceOffset->X = -30;
		LImage->Width = 20;
		LImage->Height = 20;
	}

	TListItemText * LCaption = reinterpret_cast<TListItemText*>((const_cast<TListViewItem*>(AItem))->Objects->FindObject(sCaption));
	if (LCaption == NULL)
	{
		LCaption = new TListItemText((TListItem*)AItem);
		LCaption->Name = sCaption;
		LCaption->Align = TListItemAlign::Trailing;
		LCaption->VertAlign = TListItemAlign::Trailing;
		LCaption->PlaceOffset->X = -10;
		LCaption->TextAlign = TTextAlign::Center;
		LCaption->Trimming = TTextTrimming::Character;
		LCaption->IsDetailText = True;
		LCaption->Width = 60;
		LCaption->Height = 18;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm594::SpeedButtonFillClick(TObject *Sender)
{
	LinkFillControlToField1->Active = false;
	for(int i = 1; i <= 20; i++) {
		TListViewItem * LItem = ListViewBottomDetail->Items->Add();
		LItem->Text = Format("Text %d", ARRAYOFCONST(( i)));
		LItem->Detail = Format("Detail %d", ARRAYOFCONST(( i)));
		LItem->BitmapRef = ImageRAD->Bitmap;
		LItem->Data[sCaption] = TValue::From<UnicodeString>(Format("thumb %d", ARRAYOFCONST(( i))));
		// Do the following instead of  above line.  Above line will copy the image
		dynamic_cast<TListItemImage*>(LItem->Objects->FindObject(sThumbNailName))->OwnsBitmap = false;
		dynamic_cast<TListItemImage*>(LItem->Objects->FindObject(sThumbNailName))->Bitmap = ImageRAD->Bitmap;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm594::SpeedButtonLiveBindingsClick(TObject *Sender)
{
	LinkFillControlToField1->Active = True;
}
//---------------------------------------------------------------------------

void __fastcall TForm594::ToggleEditModeClick(TObject *Sender)
{
	ListViewBottomDetail->EditMode = !ListViewBottomDetail->EditMode;
}
//---------------------------------------------------------------------------

void __fastcall TForm594::LinkFillControlToField1FilledListItem(TObject *Sender,
const IBindListEditorItem *AEditor)
{
	TPersistent * LBitmap = new TPersistent();
	String LName = "";
	int _currentIndex = (const_cast<IBindListEditorItem*>(AEditor))->CurrentIndex();
	if (_currentIndex >= 0) {
		TListViewItem * LItem = ListViewBottomDetail->Items->operator [](_currentIndex);
		TListItemImage * LThumb = reinterpret_cast<TListItemImage*>(LItem->Objects->FindObject(sThumbNailName));
		TListItemText * LCaption = reinterpret_cast<TListItemText*>(LItem->Objects->FindObject(sCaption));
		TBindSourceAdapterField * LField = this->PrototypeBindSource1->InternalAdapter->FindField("Bitmap1");
		TBindSourceAdapterField * LFieldName = this->PrototypeBindSource1->InternalAdapter->FindField("BitmapName1");
		if ((LField != NULL) && (LThumb != NULL)) {
			if ((LThumb->Bitmap != NULL) || (!LThumb->OwnsBitmap)) {
				LThumb->OwnsBitmap = True;
				LThumb->Bitmap = new TBitmap(0,0);
			}
			LBitmap = dynamic_cast<TPersistent*>(LField->GetTValue().AsObject());
			if (LBitmap != NULL ) {
				LThumb->Bitmap->Assign(LBitmap);
			}
			else {
                LThumb->Bitmap->Assign(NULL);
			}

			if ((LFieldName != NULL) && (LCaption != NULL)) {
				if (LFieldName->GetTValue().TryAsType<String>(LName)) {
					LCaption->Text = LName;
				}
				else {
                    LCaption->Text = "";
                }
			}
		}
	}
}
//---------------------------------------------------------------------------


