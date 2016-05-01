//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.DateUtils.hpp>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

#include <FMX.Dialogs.hpp>
/*#include <FMX.DialogService.Async.hpp>
#ifndef _PLAT_ANDROID
#include <FMX.DialogService.Sync.hpp>
#endif*/

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
// ---------------------------------------------------------------------------

void __fastcall TNotificationsForm::btnSendNotificationImmediatelyClick(TObject *Sender)

{
	// verify if the service is actually supported
	TNotification * Notification = NotificationC->CreateNotification();

	Notification->Name = "MyNotification";
	Notification->AlertBody = "C++Builder for Mobile is here!";
	Notification->FireDate = Now();

	// Send notification in Notification Center
	NotificationC->PresentNotification(Notification);

	// also this method is equivalent if supported
	//NotificationC->ScheduleNotification(Notification);

	// Free the notification
	Notification->DisposeOf();
}
//---------------------------------------------------------------------------

void __fastcall TNotificationsForm::SpeedButton1Click(TObject *Sender)
{
		// providing the fact that you already have a MyNotification previously issued
	NotificationC->CancelNotification("MyNotification");
}
//---------------------------------------------------------------------------

void __fastcall TNotificationsForm::SpeedButton2Click(TObject *Sender)
{
	NotificationC->CancelAll();
}

//---------------------------------------------------------------------------
void __fastcall TNotificationsForm::NotificationCReceiveLocalNotification(TObject *Sender,
		  TNotification *ANotification)
{
	if(ANotification)
		Memo1->Lines->Add(ANotification->AlertBody);
}
//---------------------------------------------------------------------------

