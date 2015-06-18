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

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)
#pragma resource ("*.iPhone4in.fmx", _PLAT_IOS)

TBaseMainForm *BaseMainForm;
//---------------------------------------------------------------------------
__fastcall TBaseMainForm::TBaseMainForm(TComponent* Owner)
	: TForm(Owner)
{
	FRawBitmap = new TBitmap(0,0);
}
//---------------------------------------------------------------------------
__fastcall TBaseMainForm::~TBaseMainForm()
{
	delete FRawBitmap;
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::UpdateEffect()
{
	if (FEffect != NULL) {
		FEffect->ValuesAsBitmap["Input"] = FRawBitmap;
		ImageContainer->Bitmap = FEffect->ValuesAsBitmap["Output"];
	}
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::DoOnChangedEffectParam(TObject *Sender)
{
	TTrackBar * TrackBarTmp = static_cast<TTrackBar*>(Sender);
	if (TrackBarTmp != NULL && FEffect != NULL) {
		FEffect->ValuesAsFloat[TrackBarTmp->TagString] = TrackBarTmp->Value;
		UpdateEffect();
	}
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::LoadFilterSettings(TFilterRec Rec)
{
	int i = 0;
	LayoutFilterSettings->DeleteChildren();
	TFilterValueRecArray Values = Rec.Values;
	for(i = 0; i < Values.Length; i++)
	{
		TFilterValueRec RecValue = Values[i];
		if (RecValue.ValueType != TFilterValueType::Float) continue;
		TTrackBar * TB = new TTrackBar(this);
		TB->Parent = LayoutFilterSettings;
		TB->Orientation = TOrientation::Vertical;
		TB->Align = TAlignLayout::Left;
		TB->Min = RecValue.Min.AsExtended();
		TB->Max = RecValue.Max.AsExtended();
		TB->Value = RecValue.Value.AsExtended();
		TB->TagString = RecValue.Name;
		TB->Tracking = false;
		TB->OnChange = DoOnChangedEffectParam;
	}
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::ActionListUpdate(TBasicAction *Action, bool &Handled)

{
	LayoutFilterSettings->Visible = !ActionResetEffect->Checked;
	ActionClearImage->Enabled = !ImageContainer->Bitmap->IsEmpty();
	ActionShowShareSheet->Enabled = !FRawBitmap->IsEmpty();
	ActionBlurEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionPixelateEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionSharpenEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionWaveEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionContrastEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionPaperSketchEffect->Enabled = !FRawBitmap->IsEmpty();
	TopHelp->Visible = FRawBitmap->IsEmpty() && ActionWaveEffect->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::SetEffect(const String AFilterName)
{
	ActionResetEffect->Checked = False;
	FEffect = TFilterManager::FilterByName(AFilterName);
	if (FEffect != NULL) {
		TFilterRec Rec = FEffect->FilterAttr();
		UpdateEffect();
		LoadFilterSettings(Rec);
	}
}
//---------------------------------------------------------------------------
void __fastcall TBaseMainForm::ActionBlurEffectExecute(TObject *Sender)
{
	SetEffect("GaussianBlur");
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionPixelateEffectExecute(TObject *Sender)
{
	SetEffect("Pixelate");
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionSharpenEffectExecute(TObject *Sender)
{
	SetEffect("Sharpen");
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionResetEffectExecute(TObject *Sender)
{
	ImageContainer->Bitmap->Assign(FRawBitmap);
	ActionResetEffect->Checked = true;
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionShowShareSheetBeforeExecute(TObject *Sender)
{
	ActionShowShareSheet->Bitmap = ImageContainer->Bitmap;
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionClearImageExecute(TObject *Sender)
{
	RemoveBtnAnimation->Start();
	FRawBitmap->SetSize(0, 0);
	ImageContainer->Bitmap->SetSize(0, 0);
	ImageContainer->Bitmap->Assign(FRawBitmap);
	ActionResetEffect->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionTakePhotoFromLibraryDidFinishTaking(TBitmap *Image)
{
	if (Image->Width > 1024) {
		auto ScaleFactor = Image->Width / 1024;
		Image->Resize(static_cast<int>(Image->Width / ScaleFactor),
			static_cast<int>(Image->Height / ScaleFactor));
	}
	FRawBitmap->Assign(Image);
	ImageContainer->Bitmap->Assign(Image);
	UpdateEffect();
}
//---------------------------------------------------------------------------


void __fastcall TBaseMainForm::ActionWaveEffectExecute(TObject *Sender)
{
	SetEffect("Wave");
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionContrastEffectExecute(TObject *Sender)
{
	SetEffect("Contrast");
}
//---------------------------------------------------------------------------

void __fastcall TBaseMainForm::ActionPaperSketchEffectExecute(TObject *Sender)
{
	SetEffect("PaperSketch");
}
//---------------------------------------------------------------------------

