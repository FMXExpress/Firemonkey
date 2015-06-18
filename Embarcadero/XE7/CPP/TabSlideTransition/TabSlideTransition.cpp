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

#include "TabSlideTransition.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TTabSlideTransitionFrmBase *TabSlideTransitionFrmBase;

// ---------------------------------------------------------------------------
__fastcall TTabSlideTransitionFrmBase::TTabSlideTransitionFrmBase
	(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
TListBox* TTabSlideTransitionFrmBase::FindListBox(TFmxObject* AObj)
{
	TListBox* Result = NULL;
	TFmxObject* Obj;
	for (int i = 0; i < AObj->ChildrenCount; ++i) {
		Obj = AObj->Children->operator [](i);
		Result = dynamic_cast<TListBox*>(Obj);
		if (Result != NULL)
			return Result;
		else
			Result = FindListBox(Obj);

		if (Result != NULL)
			return Result;
	}
	return Result;
}

// ---------------------------------------------------------------------------
bool TTabSlideTransitionFrmBase::GetIsFilled(TListBox* AList)
{
	bool Result = true;
	for (int i =0; i < AList->Count; ++i) {
		TListBoxItem* Item = AList->ItemByIndex(i);
		for (int j = 0; j < Item->ChildrenCount; ++j) {
			TFmxObject* Obj = Item->Children->operator [](j);
			TEdit* edit = dynamic_cast<TEdit*>(Obj);
			if (edit != NULL)
				Result = edit->Text != "";

			TDateEdit* dateEdit = dynamic_cast<TDateEdit*>(Obj);
			if (dateEdit != NULL)
				Result = !dateEdit->IsEmpty;

			TComboBox* combo = dynamic_cast<TComboBox*>(Obj);
			if (combo != NULL)
				Result = combo->ItemIndex >= 0;

			if (!Result)
				return Result;
		}
		if (!Result)
			return Result;
	}
	return Result;
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::UpdateKBBounds()
{
	FNeedOffset = false;
	if (Focused != NULL) {
		TControl * LFocused = (TControl *)(Focused->GetObject());
		TRectF LFocusRect = LFocused->AbsoluteRect;
		LFocusRect.Offset(VertScrollBox1->ViewportPosition.X, VertScrollBox1->ViewportPosition.Y);
		if (LFocusRect.IntersectsWith(TRectF(FKBBounds)) && (LFocusRect.Bottom > FKBBounds.Top)) {
			FNeedOffset = true;
			MainLayout1->Align = TAlignLayout::Horizontal;
			VertScrollBox1->RealignContent();
			Application->ProcessMessages();
			VertScrollBox1->ViewportPosition =
				PointF(VertScrollBox1->ViewportPosition.X,
				LFocusRect.Bottom - FKBBounds.Top);
		}
	}
	if (!FNeedOffset) {
		RestorePosition();
	}
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::RestorePosition()
{
	VertScrollBox1->ViewportPosition = PointF(VertScrollBox1->ViewportPosition.X, 0);
	MainLayout1->Align = TAlignLayout::Client;
	VertScrollBox1->RealignContent();
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::CalcContentBoundsProc
	(TObject* Sender, TRectF &ContentBounds)
{
	if (FNeedOffset && (FKBBounds.Top > 0)) {
		ContentBounds.Bottom =
			Max(ContentBounds.Bottom, 2 * ClientHeight - FKBBounds.Top);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::FormCreate(TObject *Sender)
{
	if (TPlatformServices::Current->SupportsPlatformService
		(__uuidof(IFMXVirtualKeyboardToolbarService)) && (FService =
		TPlatformServices::Current->GetPlatformService
		(__uuidof(IFMXVirtualKeyboardToolbarService))))
	{
		FService->SetToolbarEnabled(true);
		FService->SetHideKeyboardButtonVisibility(true);
	}
 	VertScrollBox1->OnCalcContentBounds = CalcContentBoundsProc;
}
// ---------------------------------------------------------------------------

void __fastcall TTabSlideTransitionFrmBase::FormVirtualKeyboardHidden
	(TObject *Sender, bool KeyboardVisible, const TRect &Bounds)
{
	FKBBounds = TRectF(0, 0, 0, 0);
	FNeedOffset = False;
	RestorePosition();
}
// ---------------------------------------------------------------------------

void __fastcall TTabSlideTransitionFrmBase::FormVirtualKeyboardShown
	(TObject *Sender, bool KeyboardVisible, const TRect &Bounds)
{
	FKBBounds = TRectF(ScreenToClient(FKBBounds.TopLeft()), ScreenToClient(FKBBounds.BottomRight()));
	UpdateKBBounds();
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::FormFocusChanged(TObject *Sender) {
	UpdateKBBounds();
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::UpdateMemo()
{
	Memo1->Lines->Clear();
	Memo1->Lines->Add("Personal information");
	Memo1->Lines->Add("Name: " + efirstName->Text + " " + eLastName->Text);
	Memo1->Lines->Add("Address: " + infoAddress->Text);
	Memo1->Lines->Add("Telephone: " + infoTelephone->Text);
	Memo1->Lines->Add("E-mail: " + infoEmail->Text);
	Memo1->Lines->Add("Date of birth:" + infoDate->Text);
	Memo1->Lines->Add("");

	Memo1->Lines->Add("Education");
	Memo1->Lines->Add(edInstitution->Text + ", " + edCity->Text + ", " +
		edAdmissionDate->Selected->Text + "-" +
		edGraduationDate->Selected->Text);
	Memo1->Lines->Add("");

	Memo1->Lines->Add("Work experience");
	Memo1->Lines->Add(weOccupiedJob->Text + " at " + weEmpName->Text + ", " +
		weCity->Text + ", " + weFrom->Selected->Text + "-" +
		weTo->Selected->Text);
}
//---------------------------------------------------------------------------

void __fastcall TTabSlideTransitionFrmBase::ActionList1Update(TBasicAction *Action, bool &Handled)
{
	TTabItem* tab = TabControl1->ActiveTab;
	if (Action == NextTabAction) {
		TListBox* list = FindListBox(tab);
		if (list != NULL)
			NextTabAction->Enabled = GetIsFilled(list);
	}

	if (Action == TitleAction)
		TitleAction->Text = TabControl1->ActiveTab->Text;

	if (tab == TabItem5)
		UpdateMemo();
}
//---------------------------------------------------------------------------

void __fastcall TTabSlideTransitionFrmBase::FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar, TShiftState Shift)

{
	if (Key == vkHardwareBack) {
		TabControl1->Previous(TTabTransition::Slide, TTabTransitionDirection::Normal);
		Key = 0;
	}
}
//---------------------------------------------------------------------------

void __fastcall TTabSlideTransitionFrmBase::TabControl1Gesture(TObject *Sender, const TGestureEventInfo &EventInfo, bool &Handled)

{
	switch (EventInfo.GestureID) {
		case sgiLeft:
			if (NextTabAction->Enabled == true)
				TabControl1->Next(TTabTransition::Slide, TTabTransitionDirection::Normal);
			Handled = true;
			break;
		case sgiRight:
			TabControl1->Previous(TTabTransition::Slide, TTabTransitionDirection::Normal);
			Handled = true;
			break;
	default:
        break;
	}
}
//---------------------------------------------------------------------------

