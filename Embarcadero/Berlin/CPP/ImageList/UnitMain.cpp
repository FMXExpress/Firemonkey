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

#include "UnitMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
  FImageLink = new TImageLink();
  FImageLink->Images = MainDataModule->ImageList1;
  FImageLink->ImageIndex = Glyph1->ImageIndex;
  FImageLink->OnChange = OnImagesChange;
  bool AVisible = false;
  // {pfWindows, pfMacOS, pfiOS, pfAndroid, pfWinRT, pfLinux };
  switch(TOSVersion::Platform) {
	  case TOSVersion::TPlatform::pfWindows:
	  case TOSVersion::TPlatform::pfMacOS:
	  case TOSVersion::TPlatform::pfWinRT:
	  case TOSVersion::TPlatform::pfLinux:
			AVisible = true;
			break;
	  case TOSVersion::pfAndroid:
	  case TOSVersion::pfiOS:
			AVisible = false;
            break;
  }
  MenuBar1->Visible = AVisible;
}
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm(void)
{
	FImageLink->DisposeOf();
}

void __fastcall TMainForm::DrawTextOnLayer(const int Index, const String Text)
{
	int LayerIndex = MainDataModule->ImageList1->Destination->Items[Index]->Layers->Count - 1;
	if(LayerIndex >= 0) {
		TLayer * Layer = MainDataModule->ImageList1->Destination->Items[Index]->Layers->Items[LayerIndex];
		String SourceName = Layer->Name;
		TSize Size;
        TCustomBitmapItem * Item;
		// Create a new source image if earlier its was not
		if(!MainDataModule->ImageList1->BitmapItemByName(SourceName, Item, Size)) {
			Size.cx = Layer->SourceRect->Rect.Width();
			Size.cy = Layer->SourceRect->Rect.Height();
			TCustomSourceItem * NewSource = MainDataModule->ImageList1->Source->Add();
			NewSource->Name = SourceName;
			Item = NewSource->MultiResBitmap->ItemByScale(1, false, true);
			if(Item == NULL) {
				Item = NewSource->MultiResBitmap->Add();
			}
			Item->Bitmap->SetSize(Size.cx, Size.cy);
		}
		// Output some text
		if(Item != NULL) {
			Item->Bitmap->Clear(TAlphaColor(0x0));
			TCanvas * canvas = Item->Bitmap->Canvas;
			if(canvas->BeginScene()) {
				__try {
					canvas->Font->Size = 15;
					canvas->Fill->Color = TAlphaColor(TAlphaColorRec::Red);
					canvas->Fill->Kind = TBrushKind::Solid;
					canvas->FillText(TRectF(1, 0, Size.cx - 1, Size.cy / 2), Text, false, 1, TFillTextFlags(),
						TTextAlign::Center, TTextAlign::Center);
				}
				__finally {
					canvas->EndScene();
				}
			}
		}
	}
}

void __fastcall TMainForm::DrawPicture(TCanvas * ACanvas, TRectF R, float Scale)
{
	if(Canvas->BeginScene()) {
		__try {
			Canvas->Font->Size = 6 * Scale;
			Canvas->Fill->Kind = TBrushKind::Solid;
			Canvas->Fill->Color = TAlphaColor(TAlphaColorRec::Aqua);
			Canvas->Stroke->Color = TAlphaColor(TAlphaColorRec::Green);
			Canvas->Stroke->Thickness = Scale;
			Canvas->DrawRect(R, Scale, Scale, AllCorners, 1, TCornerType::Bevel);
			Canvas->FillRect(R, Scale, Scale, AllCorners, 1, TCornerType::Bevel);
			Canvas->Stroke->Color = TAlphaColor(TAlphaColorRec::Black);
			Canvas->Stroke->Thickness = 1;
			for(int i = static_cast<int>(R.bottom/2 + 2 * Scale); i < static_cast<int>(R.bottom - 2 * Scale); i++) {
				if((i%2)==0) {
					Canvas->DrawLine(TPointF(R.Left + 2 * Scale, i + 0.5),
						TPointF(R.Right -2 * Scale, i + 0.5), 1);
                }
			}
			Canvas->Fill->Color =  TAlphaColor(TAlphaColorRec::Darkblue);
			Canvas->FillText(R, FloatToStr(Scale), false, 1, TFillTextFlags(), TTextAlign::Center, TTextAlign::Leading);
		}
		__finally {
			Canvas->EndScene();
        }
    }
}

void __fastcall TMainForm::AddSourceToItem(const int Index)
{
	TLayer * Layer;
    String SourceName;
	// create a new layer if need
	if(MainDataModule->ImageList1->Destination->Items[Index]->Layers->Count == 0) {
		Layer = MainDataModule->ImageList1->Destination->Items[Index]->Layers->Add();
		Layer->SourceRect->Rect = TRectF(0, 0, 16, 16);
		SourceName = "";
	}
	else {
		Layer = MainDataModule->ImageList1->Destination->Items[Index]->Layers->Items[0];
		SourceName = Layer->Name;
	}
	TCustomBitmapItem *Item;
    TSize Size;
	// Create a new source image, if it is not present yet
	if(!MainDataModule->ImageList1->BitmapItemByName(SourceName, Item, Size)) {
		TCustomSourceItem * NewSource = MainDataModule->ImageList1->Source->Add();
		if(SourceName == "") {
			Layer->Name = NewSource->Name;
		}
		else {
			NewSource->Name = SourceName;
		}
		if(NewSource->MultiResBitmap->Count == 0) {
			// Create several bitmaps for different scales
			float S = 0;
    		TRectF R;
			for(int i = 1; i <= 10; i++) {
				Item = NewSource->MultiResBitmap->Add();
				S = Item->Scale;
				Item->Bitmap->BitmapScale = 1;
				Size.cx = Layer->SourceRect->Width() * S;
				Size.cy = Layer->SourceRect->Height() * S;
				Item->Bitmap->SetSize(Size.cx, Size.cy);
				Item->Bitmap->Clear(TAlphaColor(TAlphaColor(0x0)));
				R = TRectF(S + 0.5, S + 0.5, Size.Width - S - 0.5, Size.Height - S - 0.5);
				DrawPicture(Item->Bitmap->Canvas, R, S);
			}
		}
	}
}

void __fastcall TMainForm::OnImagesChange(TObject * Sender)
{
    TabItem4->Repaint();
}

void __fastcall TMainForm::TabItem4Paint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect)
{
	TRectF R = TRect(ARect.Right - 18, 2, ARect.Right - 2, 18);
	MainDataModule->ImageList1->Draw(TabItem4->Canvas, R, FImageLink->ImageIndex);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Glyph1Changed(TObject *Sender)
{
	FChangeCount++;
  	Label2->Text = "Glyph1Changed: " + IntToStr(FChangeCount);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnUpdateTextExecute(TObject *Sender)
{
	FNumber++;
	DrawTextOnLayer(8, IntToStr(FNumber));
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnAddSourceExecute(TObject *Sender)
{
	AddSourceToItem(9);
	ActnAddSource->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnNextImageExecute(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)){
		TCustomAction * Actn = (TCustomAction*)Sender;
		if((Actn->ActionList != NULL) && (dynamic_cast<TCustomImageList*>(Actn->ActionList->Images))) {
			TCustomImageList * Images = (TCustomImageList*)Actn->ActionList->Images;
			if(Actn->ImageIndex < Images->Count - 1) {
				Actn->ImageIndex = Actn->ImageIndex + 1;
			}
			else {
                Actn->ImageIndex = -1;
            }
        }
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnDormantExecute(TObject *Sender)
{
	MainDataModule->ImageList1->Dormant = !MainDataModule->ImageList1->Dormant;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnDormantUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)){
		((TCustomAction*)Sender)->Checked = MainDataModule->ImageList1->Dormant;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnUpCacheExecute(TObject *Sender)
{
	MainDataModule->ImageList1->CacheSize = MainDataModule->ImageList1->CacheSize + 1;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnUpCacheUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)){
		((TCustomAction*)Sender)->Text =
			String().sprintf(L"Up Cache Size (%d)",
				MainDataModule->ImageList1->CacheSize);
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnDownCacheExecute(TObject *Sender)
{
	MainDataModule->ImageList1->CacheSize = MainDataModule->ImageList1->CacheSize - 1;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnDownCacheUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)) {
		((TCustomAction*)Sender)->Enabled = MainDataModule->ImageList1->CacheSize > 1;
		((TCustomAction*)Sender)->Text =
			String().sprintf(L"Down Cache Size (%d)",
				MainDataModule->ImageList1->CacheSize
			);
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnClearExecute(TObject *Sender)
{
    ListView2->Items->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnClearCacheExecute(TObject *Sender)
{
    MainDataModule->ImageList1->ClearCache();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnListViewAddExecute(TObject *Sender)
{
	if(MainDataModule->ImageList1->Count > 0) {
		ListView2->ItemAppearanceName = TAppearanceNames_ImageListItem;
		ListView2->ItemAppearanceObjects->ItemObjects->Image->Visible = true;
		TListViewItem * Item = ListView2->Items->Add();
		if (Item->Objects->ImageObject != NULL) {
			Item->Objects->ImageObject->ImageIndex = Item->Index % MainDataModule->ImageList1->Count;
			Item->Text = String().sprintf(L"Item with ImageIndex = %d", Item->Objects->ImageObject->ImageIndex);
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnListAddBitmapExecute(TObject *Sender)
{
	if(!Image1->Bitmap->IsEmpty()){
		ListView2->ItemAppearanceName = TAppearanceNames_ImageListItem;
		ListView2->ItemAppearanceObjects->ItemObjects->Image->Visible = true;
		TListViewItem * Item = ListView2->Items->Add();
		if (Item->Objects->ImageObject != NULL) {
			Item->Bitmap = Image1->Bitmap;
			Item->Text = String().sprintf(L"Item with Bitmap = \"%s\"", Image1->Name.c_str());
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnAddBitmapAndImageExecute(TObject *Sender)
{
	if((MainDataModule->ImageList1->Count > 0)&&(!Image1->Bitmap->IsEmpty())) {
        ListView2->ItemAppearanceName = TAppearanceNames_ImageListItem;
		ListView2->ItemAppearanceObjects->ItemObjects->Image->Visible = true;
		TListViewItem * Item = ListView2->Items->Add();
		if (Item->Objects->ImageObject != NULL) {
			Item->Bitmap = Image1->Bitmap;
			Item->Objects->ImageObject->ImageIndex = Item->Index % MainDataModule->ImageList1->Count;
			Item->Text = String().sprintf(L"Item with ImageIndex = %d and Bitmap = \"%s\"",
				Item->Objects->ImageObject->ImageIndex, Image1->Name.c_str());
		}
    }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnImagesExecute(TObject *Sender)
{
	if(ListView2->Images != NULL){
		ListView2->Images = NULL;
	}
	else {
		ListView2->Images = MainDataModule->ImageList1;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnImagesUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)) {
		((TCustomAction*)Sender)->Checked = ListView2->Images != NULL;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnInfoUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)) {
		String S = "Images: ";
		if(Glyph1->Images != NULL) {
			S = String().sprintf(L"Images: \"%s\" Count: %d", Glyph1->Images->Name.c_str(),
				Glyph1->Images->Count);
			if(Glyph1->Images->Dormant) {
                S += "; Dormant";
            }
		}
		else {
			S = "Images: nil";
		}
		S += String(sLineBreak) + String().sprintf(L"ImageIndex: %d; Width: %2.0f; Height: %2.0f",
			Glyph1->ImageIndex, Glyph1->Width, Glyph1->Height);
		if(Glyph1->Visible) {
			S += "; Visible";
		}
        ((TCustomAction*)Sender)->Text = S;
    }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnIncIndexExecute(TObject *Sender)
{
	if(Glyph1->Images != NULL) {
		if(Glyph1->ImageIndex < Glyph1->Images->Count - 1) {
			Glyph1->ImageIndex++;
		}
		else {
			Glyph1->ImageIndex = -1;
		}
		FImageLink->ImageIndex = Glyph1->ImageIndex;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnDecIndexExecute(TObject *Sender)
{
	if(Glyph1->Images != NULL) {
		if(Glyph1->ImageIndex >= 0) {
			Glyph1->ImageIndex--;
		}
		else {
			Glyph1->ImageIndex = Glyph1->ImageIndex -1;
		}
		FImageLink->ImageIndex = Glyph1->ImageIndex;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnShowCheckBoxExecute(TObject *Sender)
{
	TreeView1->ShowCheckboxes = !TreeView1->ShowCheckboxes;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ActnShowCheckBoxUpdate(TObject *Sender)
{
	if(dynamic_cast<TCustomAction*>(Sender)) {
		((TCustomAction*)Sender)->Checked = TreeView1->ShowCheckboxes;
	}
}
//---------------------------------------------------------------------------

