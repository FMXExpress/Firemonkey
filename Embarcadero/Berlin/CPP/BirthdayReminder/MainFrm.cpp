// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm.h"
#include "System.UITypes.hpp"
#include "System.DateUtils.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFormMain *FormMain;

// ---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::AddressBookExternalChange(TObject *ASender) {
	AddressBook->RevertCurrentChangesAndUpdate();
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::FormShow(TObject *Sender) {
	AddressBook->RequestPermission();
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::ListView1PullRefresh(TObject *Sender) {
	ListView1->PullRefreshWait = True;
	this->FillContactsList();
}

// ---------------------------------------------------------------------------
void TFormMain::FillContactsList() {
	if ((thread != NULL) && (!thread->Finished)) {
		thread->Terminate();
		thread->WaitFor();
	}
	if (thread != NULL) {
		thread->Free();
	}
	if (contacts != NULL) {
		contacts->Free();
	}
	contacts = new TAddressBookContacts();
	AddressBook->AllContacts(contacts);

	thread = new TFetchContactThread(contacts);
	thread->OnContactLoaded = ContactLoaded;
	thread->OnStart = ContactLoadingBegin;
	thread->OnTerminate = ContactLoadingEnd;
	thread->Start();
}

// ---------------------------------------------------------------------------
TBitmap* TFormMain::CreateRoundPhoto(TBitmapSurface* source) {
	int size = Min(source->Width, source->Height);
	TBitmap* bitmap = new TBitmap();
	bitmap->SetSize(size, size);
	bitmap->Clear(0);
	if (bitmap->Canvas->BeginScene()) {
		try {
			bitmap->Canvas->Fill->Bitmap->Bitmap->Assign(source);
			bitmap->Canvas->Fill->Kind = TBrushKind::Bitmap;
			bitmap->Canvas->FillEllipse(TRectF(0, 0, size, size), 1);
		}
		__finally {
			bitmap->Canvas->EndScene();
		}
	}
	return bitmap;
}

// ---------------------------------------------------------------------------
void TFormMain::PostNotification(const UnicodeString displayName,
	const TDateTime birthday, const int remainderDays) {

	TNotification* notification = NotificationCenter1->CreateNotification();
	try {
		TDateTime eventDate = birthday;
		eventDate = RecodeTime(eventDate, 11, 0, 0, 0);
		eventDate = ChangeYear(eventDate, CurrentYear());
		if (eventDate < Now())
			eventDate = ChangeYear(eventDate, CurrentYear() + 1);
		notification->RepeatInterval = TRepeatInterval::Year;
		notification->FireDate = eventDate;
		notification->AlertBody = "Don't forget to congratulate " + displayName;
		notification->EnableSound = true;
		NotificationCenter1->ScheduleNotification(notification);
	}
	__finally {
		notification->Free();
	}
}

// ---------------------------------------------------------------------------
int TFormMain::DefineRemainedDays(const TDate birthday) {
	TDateTime eventDate = ChangeYear(birthday, CurrentYear());
	int days = DaysBetween(Date(), eventDate);
	if (eventDate < Now()) {
		days = DaysInYear(eventDate) - days;
	}
	return days;
}

// ---------------------------------------------------------------------------
TDateTime TFormMain::ChangeYear(const TDateTime date,
	const unsigned short newYear) {
	unsigned short year, month, days;
	DecodeDate(date, year, month, days);
	if (month == MonthFebruary) {
		days = Min(days, DaysInAMonth(newYear, MonthFebruary));
	}
	TDateTime newDate = EncodeDate(newYear, month, days);
	ReplaceTime(newDate,date);
	return newDate;
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::ContactLoadingBegin(TObject *Sender) {
	ListView1->Items->Clear();
	ListView1->PullRefreshWait = true;
	NotificationCenter1->CancelAll();
	ProgressPanelAnimation->Inverse = false;
	ProgressPanelAnimation->Enabled = true;
	ProgressPanelAnimation->Start();
	ProgressBar->Value = 0;
	ProgressBar->Max = contacts->Count;
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::ContactLoaded(const int totalCount, const int number,
	const TDateTime birthday, const UnicodeString displayName,
	TBitmapSurface* photo) {
	TListViewItem* item;
	int remainderDays;

	if (!IsNan(birthday.Val)) {
		remainderDays = DefineRemainedDays(birthday);
		item = ListView1->Items->Add();
		item->Detail = FormatDateTime("dd mmmm", birthday);
		item->Text = displayName;
		item->Tag = remainderDays;
		TVarRec vr[] = {remainderDays};
		item->ButtonText = Format("%d days", vr, 1);
		if (photo == NULL) {
			item->ImageIndex = 0;
		}
		else {
			item->Bitmap = CreateRoundPhoto(photo);
		}
		PostNotification(displayName, birthday, remainderDays);
	}
	ProgressBar->Value = number + 1; // Number start from 0, so we need to add 1
	TVarRec vr[] = {number, totalCount};
	LabelProgress->Text = Format("Loaded %d from %d contacts", vr, 2);
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::ContactLoadingEnd(TObject* Sender) {
	ListView1->PullRefreshWait = false;
	ProgressPanelAnimation->Inverse = true;
	ProgressPanelAnimation->Start();
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::SpeedButton1Click(TObject *Sender) {
	ListView1->SearchVisible = !ListView1->SearchVisible;
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::AddressBookPermissionRequest(TObject *ASender,
	const UnicodeString AMessage, const bool AAccessGranted) {
	if (AAccessGranted) {
		FillContactsList();
	}
	else {
		ProgressPanelAnimation->Enabled = true;
		ProgressPanelAnimation->Start();
		ProgressBar->Visible = false;
		LabelProgress->Text = "User not allowed to read contacts. " + AMessage;
	}
}
// ---------------------------------------------------------------------------
