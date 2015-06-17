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

#include "VirtualKeyboardBase.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TVKBaseForm *VKBaseForm;
//---------------------------------------------------------------------------
__fastcall TVKBaseForm::TVKBaseForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TVKBaseForm::CalcContentBoundsProc(TObject * Sender, System::Types::TRectF &ContentBounds)
{
	if (FNeedOffset && FKBBounds.Top > 0) {
		ContentBounds.Bottom = Max(ContentBounds.Bottom, 2 * ClientHeight - FKBBounds.Top);
	}
}
//---------------------------------------------------------------------------
void __fastcall TVKBaseForm::FormCreate(TObject *Sender)
{
	if (TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXVirtualKeyboardToolbarService)) &&
		(FService1 = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXVirtualKeyboardToolbarService)))) {
		FService1->SetToolbarEnabled(True);
		FService1->SetHideKeyboardButtonVisibility(True);
	}
	VertScrollBox1->OnCalcContentBounds = CalcContentBoundsProc;
}
//---------------------------------------------------------------------------

void __fastcall TVKBaseForm::FormFocusChanged(TObject *Sender)
{
	this->UpdateKBBounds();
}
//---------------------------------------------------------------------------

void __fastcall TVKBaseForm::FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds)
{
	FKBBounds = TRectF(0,0,0,0);
	FNeedOffset = False;
	RestorePosition();
}
//---------------------------------------------------------------------------

void __fastcall TVKBaseForm::FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds)
{
	FKBBounds = TRectF(Bounds);
	FKBBounds = TRectF(ScreenToClient(FKBBounds.TopLeft()),ScreenToClient(FKBBounds.BottomRight()));
	UpdateKBBounds();
}
//---------------------------------------------------------------------------

void __fastcall TVKBaseForm::UpdateKBBounds()
{
	FNeedOffset = false;
	if (Focused != NULL) {
		TControl * LFocused = static_cast<TControl*>(Focused->GetObject());
		TRectF LFocusRect = LFocused->AbsoluteRect;
		LFocusRect.Offset(VertScrollBox1->ViewportPosition.X, VertScrollBox1->ViewportPosition.Y);
		if (LFocusRect.IntersectsWith(FKBBounds) && LFocusRect.Bottom > FKBBounds.Top) {
			FNeedOffset = true;
			MainLayout1->Align = TAlignLayout::Horizontal;
			VertScrollBox1->RealignContent();
			Application->ProcessMessages();
			VertScrollBox1->ViewportPosition = PointF(VertScrollBox1->ViewportPosition.X,
				LFocusRect.Bottom - FKBBounds.Top);
		}
	}
	if (!FNeedOffset) {
		RestorePosition();
	}
}
//---------------------------------------------------------------------------

void __fastcall TVKBaseForm::RestorePosition()
{
	VertScrollBox1->ViewportPosition = PointF(VertScrollBox1->ViewportPosition.X, 0);
	MainLayout1->Align = TAlignLayout::Client;
	VertScrollBox1->RealignContent();
}
//---------------------------------------------------------------------------

