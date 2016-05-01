//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <SysUtils.hpp>
#include <System.hpp>
#pragma hdrstop

#include "unitSearchMenuHelperCpp.h"

// ---------------------------------------------------------------------------
#pragma package(smart_init)

//----------------------------------------------------------------------------
//  TSearchItem
//----------------------------------------------------------------------------

void __fastcall TSearchItem::InternalOnDblClick(TObject *Sender)
{
  if (FOnDblClick != NULL) {
	FOnDblClick(this);
  }
}
//----------------------------------------------------------------------------

void __fastcall TSearchItem::SetSearchText(String Value)
{
  FSearchText->DelimitedText = LowerCase(Value);
}

//----------------------------------------------------------------------------
String __fastcall TSearchItem::GetSearchText()
{
  return(this->FSearchText->Text);
}

//----------------------------------------------------------------------------
String __fastcall TSearchItem::GetText()
{
  return(this->FooterLabel->Text);
}

//----------------------------------------------------------------------------
void __fastcall TSearchItem::SetSearchState(TSearchState Value)
{
  FSearchState = Value;  // * is this right?
  switch (FSearchState) {
	case ssNone: 	{
						this->Opacity = 1;
						GlowPartial->Enabled = False;
						BackGroundGlow->Fill->Color = BackGroundRec->Fill->Color;
						break;
					};
	case ssNoMatch:	{
						this->Opacity = 0.5;
						GlowPartial->Enabled = False;
						BackGroundGlow->Fill->Color = BackGroundRec->Fill->Color;
						break;
					};
	case ssPartial: {
						this->Opacity = 1;
						GlowPartial->Enabled = True;
						BackGroundGlow->Fill->Color = BackGroundRec->Fill->Color;
						break;
					};
	case ssFull: 	{
						this->Opacity = 1;
						GlowPartial->Enabled = True;
						BackGroundGlow->Fill->Color = GlowPartial->GlowColor;
						break;
					};
  }
}

//----------------------------------------------------------------------------
void TSearchItem::TextSearch(String Value)
{
  if (Value == "") {
	this->State = ssNone;
	return;
  };

  String ValueStr = LowerCase(Value);

  if (Pos(ValueStr,SearchText) == 0) {
	State = ssNoMatch;
	return;
  };

  if (FSearchText->IndexOf(ValueStr) > -1) {
	State = ssFull;
  }
  else
  {
	State = ssPartial;
  };
}

//----------------------------------------------------------------------------
TSearchItem::TSearchItem(TComponent *AOwner, TAlphaColor BackGroundColor, TAlphaColor GlowColor, TImage *SourceImage, String aText) : TLayout(AOwner)
{
	FSearchText = new TStringList;

	/// Make the background Blend in
	BackGroundRec = new TRectangle(this);
	BackGroundRec->Parent = this;
	BackGroundRec->Align = TAlignLayout::Client;
	BackGroundRec->Fill->Color = BackGroundColor;
	BackGroundRec->Fill->Kind = TBrushKind::Solid;
	BackGroundRec->Stroke->Color = BackGroundColor; //TAlphaColorRec->Red;//
	BackGroundRec->Visible = True;
	BackGroundRec->HitTest = True;
	BackGroundRec->OnDblClick = InternalOnDblClick;
	BackGroundRec->ClipChildren = false;

	BackGroundGlow = new TCircle(this);
	BackGroundGlow->Parent = BackGroundRec;
	BackGroundGlow->HitTest = False;

	BackGroundGlow->Stroke->Color = BackGroundColor;
	BackGroundGlow->Fill->Color = BackGroundColor;
	BackGroundGlow->Stroke->Kind = TBrushKind::Solid;
	BackGroundGlow->Fill->Kind = TBrushKind::Solid;

	BackGroundGlow->Opacity = 0.85;
	BackGroundGlow->Align = TAlignLayout::Client;

	// Add the image
	Image  = new TImage(this);
	Image->Parent = BackGroundRec; //BackGroundGlow;//
	Image->Width = BackGroundRec->Width;
	Image->Height = BackGroundRec->Height;
	Image->Padding->Left = 20;
	Image->Padding->Right = 20;
	Image->Padding->Top = 20;
	Image->Padding->Bottom = 20;
	Image->Align = TAlignLayout::Center;
	Image->HitTest = False;

	TMemoryStream *Stream = new TMemoryStream;
	try
	{
		SourceImage->Bitmap->SaveToStream(Stream);
		Stream->Position = 0;
		Image->Bitmap->LoadFromStream(Stream);
	}
	__finally
	{
		delete(Stream);
	}

	Image->Visible = True;

	// Add the two glow effects to the image
	GlowPartial = new TGlowEffect(this);
	GlowPartial->Parent = Image;
	GlowPartial->Opacity = 1;
	GlowPartial->Softness = 1;
	//GlowPartial->ShadowColor = GlowColor;
	GlowPartial->GlowColor = GlowColor;
	GlowPartial->Enabled = False;

	FooterLabel = new TLabel(this);
	FooterLabel->Parent = this;
	FooterLabel->Align = TAlignLayout::Bottom;
	FooterLabel->TextAlign = TTextAlign::Center;
	FooterLabel->Text = aText;
	FooterLabel->Font->Size = 10;
	FooterLabel->Height = 20;
	FooterLabel->Visible = True;
	FooterLabel->HitTest = False;
}

//----------------------------------------------------------------------------
// Destructor
__fastcall TSearchItem::~TSearchItem ()
{
	delete(GlowPartial);
	delete(Image);
	delete(BackGroundGlow);
	delete(BackGroundRec);
	delete(FooterLabel);
//
//	GlowPartial = NULL;
//	Image = NULL;
//	BackGroundGlow = NULL;
//	BackGroundRec = NULL;
//	FooterLabel = NULL;

	//How does inherited ~TLayout get called?
	//TLayout::~TLayout();
}

//----------------------------------------------------------------------------
//  TSearchBand
//----------------------------------------------------------------------------

float __fastcall TSearchBand::GetItemHeight()
{
  return(this->FItemGrid->ItemHeight);
}
float __fastcall TSearchBand::GetItemWidth()
{
  return(this->FItemGrid->ItemWidth);
}
void __fastcall TSearchBand::SetItemHeight(const float Value)
{
  this->FItemGrid->ItemHeight = Value;
}
void __fastcall TSearchBand::SetItemWidth(const float Value)
{
  this->FItemGrid->ItemWidth = Value;
}
int __fastcall TSearchBand::GetCount()
{
  return(this->FSearchItems->Count);
}
void __fastcall TSearchBand::InternalResize(TObject* Sender)
{
  this->Height = this->FItemGrid->ItemHeight;

  float Ratio = (this->ItemWidth * this->Count) / this->Width;
  this->Height = this->FBandTitle->Height + (ItemHeight * Ceil(Ratio)) + 30; //Bottom Padding
}

String __fastcall TSearchBand::GetText()
{
  return(this->FBandTitle->Text);
}

TSearchBand::TSearchBand(TComponent *AOwner, bool aOwnsObjects, TAlphaColor BackGroundColor, String aText, float aItemHeight, float aItemWidth) : TLayout(AOwner)
{
  //this->FSearchItems = TObjectList<TSearchItem>.Create(aOwnsObjects);
  this->FSearchItems = new TObjectList(aOwnsObjects);
  this->Align = TAlignLayout::Top;

  FBandTitle = new TLabel(this);
  FBandTitle->Parent = this;
  FBandTitle->Text = aText;
  FBandTitle->Visible = True;
  FBandTitle->Align = TAlignLayout::Top;
  FBandTitle->Font->Size = 16;
  FBandTitle->Font->Style =  TFontStyles() << fsBold;
  FBandTitle->Padding->Top = 10;
  FBandTitle->Padding->Bottom = 10;

  FItemGrid = new TGridLayout(this);
  this->FItemGrid->Parent = this;
  this->FItemGrid->ItemHeight = aItemHeight;
  this->FItemGrid->ItemWidth = aItemWidth;
  this->FItemGrid->Align = TAlignLayout::Client;
  this->FItemGrid->ClipChildren = false;

  this->FItemGrid->OnResize = &(this->InternalResize);
  InternalResize(this->FItemGrid);
}

__fastcall TSearchBand::~TSearchBand ()
{
  delete(FBandTitle);
  delete(FSearchItems);
  delete(FItemGrid);
//  FBandTitle = NULL;
//  FSearchItems = NULL;
//  FItemGrid = NULL;
}

void TSearchBand::TextSearch(const String Value)
{
  for(int i=0; i<Count; i++) {
	TSearchItem *Obj = static_cast<TSearchItem*>(FSearchItems->Items[i]);   //??
	Obj->TextSearch(Value);
  }
}

void TSearchBand::Add(TSearchItem *Item)
{
  if (Item == NULL) {
	return;
  }

  // Add New Items Only
  if (FSearchItems->IndexOf(Item) > -1) {
	return;
  }

  FItemGrid->AddObject(Item);
  FSearchItems->Add(Item);

  InternalResize(FItemGrid);
}

//----------------------------------------------------------------------------
//   TSearchBandManager
//----------------------------------------------------------------------------

TSearchBand * TSearchBandManager::GetSBItem(int index)
{
	TSearchBand *Obj = dynamic_cast<TSearchBand*>(FSearchBands->Items[index]);
	return(Obj);
}

int __fastcall TSearchBandManager::GetCount()
{
	return(FSearchBands->Count);
}

TSearchBandManager::TSearchBandManager (TComponent *AOwner, bool aOwnsObjects) : TLayout(AOwner)
{
  this->FSearchBands = new TObjectList(aOwnsObjects);
}

__fastcall TSearchBandManager::~TSearchBandManager()
{
   delete(this->FSearchBands);
//  FSearchBands = NULL;
}

void TSearchBandManager::Add(TSearchBand *SearchBand)
{
  if (SearchBand == NULL) {
	return;
  }

  if (this->FSearchBands->IndexOf(SearchBand) > -1) {
	return;
  }

  if (Count > 0) {
	for (int i=0; i<Count-1; i++) {
	  TSearchBand *Band = Items[i];
	  Band->Align = TAlignLayout::MostTop;
	}
  }

  this->FSearchBands->Add(SearchBand);
  SearchBand->Parent = this;
  SearchBand->Align = TAlignLayout::Top;
  SearchBand->Visible = True;

  // add it to the bottom
  if (this->Count > 0)
  {
	TSearchBand *bottomItem = Items[Count-1];
	SearchBand->Top = (bottomItem->Top + bottomItem->Height);
  }
  else
  {
	SearchBand->Top = 0.0;
  }
}

void TSearchBandManager::TextSearch(String Value)
{
  for(int i=0; i<Count; i++) {
	TSearchBand *Obj = Items[i];
	Obj->TextSearch(Value);
  }
}

void TSearchBandManager::Clear()
{
   FSearchBands->Clear();
}

TSearchBand *TSearchBandManager::BandByName(String name)
{

  for(int i=0; i<Count; i++) {
	TSearchBand *Obj = this->Items[i];
	if (Obj->Text == name) {
	  return(Obj);
	};
  }
  return(NULL);
}

