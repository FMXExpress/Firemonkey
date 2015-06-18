// ---------------------------------------------------------------------------

#pragma hdrstop

#include "FmxMultiViewCustomPresentation.h"
#include <memory>
#pragma package(smart_init)

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::DoFormReleased
	(System::TObject* const Sender, System::Messaging::TMessage* const M) {
	if (Sender == FDetailOverlay->Parent) {
		FDetailOverlay->Parent = NULL;
	}
}
// ---------------------------------------------------------------------------

System::UnicodeString __fastcall
	TMultiViewAlertPresentation::GetDisplayName(void) {
	return L"Alert Window";
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::DoInstall(void) {
	MultiView->Visible = false;
	MultiView->Align = TAlignLayout::None;
	if (MultiView->Scene != NULL)
		FDetailOverlay->Parent = static_cast<TCommonCustomForm*>
			(MultiView->Scene->GetObject());
	if (MultiView->HasMasterButton())
		MultiView->MasterButton->Visible = true;
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::DoUninstall(void) {
	MultiView->Visible = true;
	FDetailOverlay->Parent = NULL;
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::DoOpen(const float ASpeed) {
	// Install content into Alert Panel
	FFrame->Opacity = 0;
	FFrame->Width = MultiView->Width;
	FFrame->Height = MultiView->PopoverOptions->PopupHeight;
	if (MultiView->Scene != NULL) {
		TCommonCustomForm * SceneForm =
			static_cast<TCommonCustomForm*>(MultiView->Scene->GetObject());
		FFrame->Parent = SceneForm;
		FFrame->Position->Point =
			TPointF(SceneForm->Width / 2 - FFrame->Width / 2,
			SceneForm->Height / 2 - FFrame->Height / 2);
	}
	MultiView->MasterContent->Parent = FFrame;
	FDetailOverlay->Visible = true;
	TAnimator::AnimateFloat(FDetailOverlay, "opacity",
		MultiView->ShadowOptions->Opacity,
		MultiView->DrawerOptions->DurationSliding);
	TAnimator::AnimateFloat(FFrame, "opacity", 1,
		MultiView->DrawerOptions->DurationSliding);
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::DoClose(const float ASpeed) {
	FFrame->Parent = NULL;
	FDetailOverlay->Visible = false;
	MultiView->MasterContent->Parent = MultiView;
}

// ---------------------------------------------------------------------------
/* Mouse events */
void __fastcall TMultiViewAlertPresentation::DoMouseDown(TObject *Sender,
	TMouseButton Button, TShiftState Shift, float X, float Y) {
	Close();
}

// ---------------------------------------------------------------------------
__fastcall TMultiViewAlertPresentation::TMultiViewAlertPresentation
	(TCustomMultiView* AMultiView) : TMultiViewPresentation(AMultiView) {

	TMessageManager::DefaultManager->SubscribeToMessage
		(__classid(TFormReleasedMessage), &DoFormReleased);
	/* Detail overlay layer for catching mouse events */
	FDetailOverlay = new TShadowedOverlayLayout(NULL);
	FDetailOverlay->Stored = false;
	FDetailOverlay->Mode = TCustomOverlayLayout::TOverlayMode::AllLocalArea;
	FDetailOverlay->EnabledShadow = MultiView->ShadowOptions->Enabled;
	FDetailOverlay->Color = MultiView->ShadowOptions->Color;
	FDetailOverlay->Opacity = 0;
	FDetailOverlay->Align = TAlignLayout::Contents;
	FDetailOverlay->Lock();
	FDetailOverlay->Visible = false;
	FDetailOverlay->OnMouseDown = &DoMouseDown;

	FFrame = new TPanel(NULL);
	FFrame->Padding->Rect = TRectF(1, 1, 1, 1);
}

// ---------------------------------------------------------------------------
__fastcall TMultiViewAlertPresentation::~TMultiViewAlertPresentation(void) {
	TMessageManager::DefaultManager->Unsubscribe
		(__classid(TFormReleasedMessage), &DoFormReleased);
	FDetailOverlay->Free();
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::Realign(void) {
	if (MultiView->Scene != NULL) {
		TCommonCustomForm * SceneForm =
			static_cast<TCommonCustomForm*>(MultiView->Scene->GetObject());
		FFrame->Position->Point =
			TPointF(SceneForm->Width / 2 - FFrame->Width / 2,
			SceneForm->Height / 2 - FFrame->Height / 2);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMultiViewAlertPresentation::UpdateSettings(void) {
	if (!Opened())
		FDetailOverlay->Opacity = 0;
	else
		FDetailOverlay->Opacity = MultiView->ShadowOptions->Opacity;
	FDetailOverlay->EnabledShadow = MultiView->ShadowOptions->Enabled;
	FDetailOverlay->Color = MultiView->ShadowOptions->Color;
}
// ---------------------------------------------------------------------------
