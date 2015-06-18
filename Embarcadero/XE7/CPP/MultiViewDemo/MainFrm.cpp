// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender) {
	MultiView1->CustomPresentationClass =
		__classid(TMultiViewAlertPresentation);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::cbMultiViewModeChange(TObject *Sender) {
	if (cbMultiViewMode->ItemIndex >= 0) {
		MultiView1->Mode = TMultiViewMode(cbMultiViewMode->ItemIndex);
	}
}
// ---------------------------------------------------------------------------
void __fastcall TForm1::swShadowSwitch(TObject *Sender)
{
	MultiView1->ShadowOptions->Enabled = swShadow->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::nbShadowOpacityChangeTracking(TObject *Sender)
{
	MultiView1->ShadowOptions->Color = cbShadowColor->Color;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::cbShadowColorChange(TObject *Sender)
{
	MultiView1->ShadowOptions->Color = cbShadowColor->Color;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::cbSlidingModeChange(TObject *Sender)
{
	if (cbSlidingMode->ItemIndex >= 0)
	MultiView1->DrawerOptions->Mode = TSlidingMode(cbSlidingMode->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::nbTouchAreaSizeChange(TObject *Sender)
{
	MultiView1->DrawerOptions->TouchAreaSize = nbTouchAreaSize->Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::cbSlidingDirectionChange(TObject *Sender)
{
	if (cbSlidingDirection->ItemIndex >= 0)
		MultiView1->DrawerOptions->Placement = TPanelPlacement(cbSlidingDirection->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::nbDurationSlidingChange(TObject *Sender)
{
	MultiView1->DrawerOptions->DurationSliding = nbDurationSliding->Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::nmAppearanceDurationChange(TObject *Sender)
{
	MultiView1->PopoverOptions->AppearanceDuration = nmAppearanceDuration->Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::cbPopoverTintChange(TObject *Sender)
{
	MultiView1->PopoverOptions->TintColor = cbPopoverTint->Color;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::nbPopupHeightChange(TObject *Sender)
{
	MultiView1->PopoverOptions->PopupHeight = nbPopupHeight->Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::cbSplitViewPlacementChange(TObject *Sender)
{
	if (cbSplitViewPlacement->ItemIndex >= 0)
		MultiView1->SplitViewOptions->Placement = TPanelPlacement(cbSplitViewPlacement->ItemIndex);
}
//---------------------------------------------------------------------------

