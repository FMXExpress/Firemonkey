
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#include <System.DateUtils.hpp>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TNotificationsForm *NotificationsForm;

// ---------------------------------------------------------------------------
__fastcall TNotificationsForm::TNotificationsForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TNotificationsForm::btnSendScheduledNotificationClick
	(TObject *Sender)

{
	// verify if the service is actually supported
	if (NotificationC->Supported()) {
		TNotification * Notification = NotificationC->CreateNotification();

		Notification->Name = "MyNotification";
		Notification->AlertBody = "C++Builder for Mobile is here!";

		/* Fired in 10 second */
		Notification->FireDate = System::Dateutils::IncSecond(Now(), 10);

		/* Send notification in Notification Center */
		NotificationC->ScheduleNotification(Notification);

		/* Free the memory */
		Notification->DisposeOf();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TNotificationsForm::btnSendNotificationImmediatelyClick(TObject *Sender)

{
	// verify if the service is actually supported
	if(NotificationC->Supported()){
		TNotification * Notification = NotificationC->CreateNotification();

		Notification->Name = "MyNotification";
		Notification->AlertBody = "C++Builder for Mobile is here!";
		Notification->FireDate = Now();

		// Send notification in Notification Center
		NotificationC->ScheduleNotification(Notification);
		// also this method is equivalent

		// Free the notification
		Notification->DisposeOf();
	}
}
//---------------------------------------------------------------------------

void __fastcall TNotificationsForm::SpeedButton1Click(TObject *Sender)
{
	if (NotificationC->Supported()){
		// providing the fact that you already have a MyNotification previously issued
		NotificationC->CancelNotification("MyNotification");
	}
}
//---------------------------------------------------------------------------

void __fastcall TNotificationsForm::SpeedButton2Click(TObject *Sender)
{
	if (NotificationC->Supported()) {
        NotificationC->CancelAll();
	}
}
//---------------------------------------------------------------------------

