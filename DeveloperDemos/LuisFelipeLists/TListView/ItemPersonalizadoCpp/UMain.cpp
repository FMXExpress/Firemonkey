//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "UMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm6 *Form6;
//---------------------------------------------------------------------------
__fastcall TForm6::TForm6(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm6::ListView1UpdateObjects(TObject * const Sender, TListViewItem * const AItem)
{
	// Debemos colocar codigo aqui.
	TListItemText * AItemText;
	TListItemImage * AItemImage;

	AItemText = reinterpret_cast<TListItemText*>(const_cast<TListViewItem*>(AItem)->Objects->FindObject("Salary"));
	if(AItemText == NULL) {
		AItemText = new TListItemText((TListItem*)AItem);
		AItemText->Name = "Salary";
		AItemText->Align = TListItemAlign::Trailing;
		AItemText->TextAlign = TTextAlign::Trailing;
		AItemText->PlaceOffset->X = -30;
		AItemText->PlaceOffset->Y = 0;
		AItemText->Width = 120;
	}

	AItemText = reinterpret_cast<TListItemText*>(const_cast<TListViewItem*>(AItem)->Objects->FindObject("HireDate"));
	if(AItemText == NULL) {
		AItemText = new TListItemText((TListItem*)AItem);
		AItemText->Name = "HireDate";
		AItemText->Align = TListItemAlign::Trailing;
		AItemText->TextAlign = TTextAlign::Trailing;
		AItemText->PlaceOffset->X = -30;
		AItemText->PlaceOffset->Y = 20;
		AItemText->Width = 120;
	}

	AItemImage = reinterpret_cast<TListItemImage*>(const_cast<TListViewItem*>(AItem)->Objects->FindObject("MiImage"));
	if(AItemImage == NULL) {
		AItemImage = new TListItemImage((TListItem*)AItem);
		AItemImage->Name = "MiImage";
		AItemImage->Align = TListItemAlign::Trailing;
		AItemImage->VertAlign = TListItemAlign::Center;
		AItemImage->ScalingMode = TImageScalingMode::Stretch;
		AItemImage->PlaceOffset->X = -200;
		AItemImage->Width = 24;
		AItemImage->Height = 24;

	}
}
//---------------------------------------------------------------------------
void __fastcall TForm6::LinkFillControlToField1FilledListItem(TObject *Sender,
	IBindListEditorItem * const AEditor)
{
	TListViewItem * AItem;
	TListItemText * AItemText;
	TListItemImage * AItemImage;
	TField * AField;

	int posicion = const_cast<IBindListEditorItem*>(AEditor)->CurrentIndex();
	// Debemos colocar codigo aquí tambien
	if(posicion >= 0) {
		AItem = ListView1->Items->Item[posicion];

		AItemText = reinterpret_cast<TListItemText*>(AItem->Objects->FindObject("Salary"));
		AField = BindSourceDB1->DataSet->FieldByName("Salary");
		if((AItem != NULL) && (AField != NULL)) {
			AItemText->Text = "Bs. " + FormatFloat("#,#00.00", AField->AsFloat);
			if(AField->AsFloat >= 45000) {
				AItemText->TextColor = TAlphaColor(TAlphaColorRec::Blue);
			} else {
				AItemText->TextColor = TAlphaColor(TAlphaColorRec::Red);
            }
		}

		AItemText = reinterpret_cast<TListItemText*>(AItem->Objects->FindObject("HireDate"));
		AField = BindSourceDB1->DataSet->FieldByName("HireDate");
		if((AItem != NULL) && (AField != NULL)) {
			AItemText->Text = FormatDateTime("dd/MM/yyyy", AField->AsDateTime);
		}

		AItemImage = reinterpret_cast<TListItemImage*>(AItem->Objects->FindObject("MiImage"));
		AItemImage->Bitmap = Image1->Bitmap;
	}
}
//---------------------------------------------------------------------------
