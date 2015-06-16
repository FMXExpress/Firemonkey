// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "TabSlideTransition.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TTabSlideTransitionFrmBase *TabSlideTransitionFrmBase;

// ---------------------------------------------------------------------------
__fastcall TTabSlideTransitionFrmBase::TTabSlideTransitionFrmBase
	(TComponent* Owner) : TForm(Owner) {
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
			MainLayout1->Align = TAlignLayout::alHorizontal;
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
	MainLayout1->Align = TAlignLayout::alClient;
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
void __fastcall TTabSlideTransitionFrmBase::edChangeTracking(TObject *Sender)
{
  Button5->Enabled = (edInstitution->Text != "") &&
		(edAdmissionDate->ItemIndex != -1) && (edCity->Text != "") &&
		(edGraduationDate->ItemIndex != -1);
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::eUserChangeTracking(TObject *Sender)
{
	Button1->Enabled = (eFirstName->Text != "") && (eLastName->Text != "");
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::infoChangeTracking(TObject *Sender)
{
	Button3->Enabled = (infoAddress->Text != "") && (infoTelephone->Text != "")
		&& (infoDate->Text != "") && (infoEmail->Text != "");
}

void __fastcall TTabSlideTransitionFrmBase::TabControl1Gesture(TObject *Sender,
	const TGestureEventInfo &EventInfo, bool &Handled) {
#ifdef ANDROID
	switch (EventInfo.GestureID) {
	case sgiLeft:
		if ((TabControl1->TabIndex == 0) && (Button1->Enabled == true))
			ChangeTabAction1->ExecuteTarget(this);
		else if ((TabControl1->TabIndex == 1) && (Button3->Enabled == true)) {
			ChangeTabAction3->ExecuteTarget(this);
		}
		else if ((TabControl1->TabIndex == 2) && (Button5->Enabled == true)) {
			ChangeTabAction5->ExecuteTarget(this);
		}
		else if ((TabControl1->TabIndex == 3) && (Button7->Enabled == true)) {
			UpdateMemo();
			ChangeTabAction7->ExecuteTarget(this);
		}
		break;
	case sgiRight:
		if (TabControl1->TabIndex == 1) {
			ChangeTabAction2->ExecuteTarget(this);
		}
		else if (TabControl1->TabIndex == 2) {
			ChangeTabAction4->ExecuteTarget(this);
		}
		else if (TabControl1->TabIndex == 3) {
			ChangeTabAction6->ExecuteTarget(this);
		}
		else if (TabControl1->TabIndex == 4) {
			ChangeTabAction8->ExecuteTarget(this);
		}
		break;
	default: ;
	}
	Handled = true;
#endif
}

// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::UpdateMemo()
{
	Memo1->Lines->Clear();
	Memo1->Lines->Add("Personal information");
	Memo1->Lines->Add("Name: " + eFirstName->Text + " " + eLastName->Text);
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
// ---------------------------------------------------------------------------
void __fastcall TTabSlideTransitionFrmBase::weChangeTracking(TObject *Sender)
{
	Button7->Enabled = (weEmpName->Text != "") && (weCity->Text != "") &&
		(weOccupiedJob->Text != "") && (weFrom->ItemIndex != -1) && (weTo->ItemIndex != -1);
	if (Button7->Enabled) {
		UpdateMemo();
	}
}

