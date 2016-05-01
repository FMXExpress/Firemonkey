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

#include "formMain.h"
#include "formData.h"
#include "dataData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  LoadData(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::LoadImagesClick(TObject *Sender)
{
   //ShowMessage("Function Not Implemented Yet - LoadImages");
   if (OpenDialog1->Execute()) {
	   dtmdlData->cdsIconData->Close();
	   dtmdlData->cdsIconData->FileName = OpenDialog1->FileName;
	   dtmdlData->cdsIconData->Open();
   }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ManageDataClick(TObject *Sender)
{
   frmData->Show();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edtSearchKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
		  TShiftState Shift)
{
   if (this->SearchBandManager == NULL) {
	 return;
   }

   this->SearchBandManager->TextSearch(this->edtSearch->Text);
}
//---------------------------------------------------------------------------
TSearchBand* __fastcall TfrmMain::InitializeBandManager()
{
   if (this->SearchBandManager == NULL) {
	  this->SearchBandManager = new TSearchBandManager(this, True);
	  this->SearchBandManager->Parent = VertScrollBox1;
	  SearchBandManager->Visible = True;
	  SearchBandManager->Align = TAlignLayout::Client;
   }
   return(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::LoadData(TObject* Sender)
{
  InitializeBandManager();
  this->SearchBandManager->Clear();

  TSearchBand* Band;

  // Load the image in. (to random sized bands)
  dtmdlData->cdsIconData->First();
  while (!dtmdlData->cdsIconData->Eof) {
	Band = SearchBandManager->BandByName(UpperCase(dtmdlData->cdsIconDataCategory->AsString));
	if (Band == NULL) {
		Band = new TSearchBand(this,True,this->Fill->Color,UpperCase(dtmdlData->cdsIconDataCategory->Text),100,100);
		SearchBandManager->Add(Band);
	}

	{
		TImage *Image = new TImage(NULL);
		Image->Bitmap->Assign(dtmdlData->cdsIconDataIcon);
		TSearchItem *SearchItem = new TSearchItem(this,this->Fill->Color,TAlphaColorRec::White,Image,dtmdlData->cdsIconDataDescription->Text);
		SearchItem->SearchText = dtmdlData->cdsIconDataSearchTerms->AsString;
		SearchItem->OnDblClick = ItemSelected;
		Band->Add(SearchItem);
		delete(Image);
	}

	dtmdlData->cdsIconData->Next();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ItemSelected(TObject *Sender)
{
   TSearchItem *Obj = dynamic_cast<TSearchItem*>(Sender);
   ShowMessage("Search Text: "+Obj->SearchText);
}

void __fastcall TfrmMain::edtSearchChange(TObject *Sender)
{
  if (((TClearingEdit *)(Sender))->Text == "") {
    this->SearchBandManager->TextSearch(this->edtSearch->Text);
  }
}
//---------------------------------------------------------------------------

