//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Notification.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <System.Notification.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
class TNotificationsForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnSendScheduledNotification;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnSendNotificationImmediately;
	TToolBar *ToolBar2;
	TSpeedButton *SpeedButton1;
	TSpeedButton *SpeedButton2;
	TNotificationCenter *NotificationC;
	TMemo *Memo1;
	void __fastcall btnSendScheduledNotificationClick(TObject *Sender);
	void __fastcall btnSendNotificationImmediatelyClick(TObject *Sender);
	void __fastcall SpeedButton1Click(TObject *Sender);
	void __fastcall SpeedButton2Click(TObject *Sender);
	void __fastcall NotificationCReceiveLocalNotification(TObject *Sender, TNotification *ANotification);

private:	// User declarations
public:		// User declarations
	__fastcall TNotificationsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TNotificationsForm *NotificationsForm;
//---------------------------------------------------------------------------
#endif
