//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotificationsUnitH
#define NotificationsUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Notification.hpp>
//---------------------------------------------------------------------------
class TNotificationsApp : public TForm
{
__published:	// IDE-managed Components
	TButton *btnShow;
	TButton *btnCancelAll;
	TButton *btnShowAnother;
	TButton *btnCancelAnother;
	TButton *btnCancel;
	TMemo *mmLog;
	TLabel *lblLog;
	TStyleBook *StyleBook1;
	TNotificationCenter *NotificationCenter1;
	void __fastcall btnShowClick(TObject *Sender);
	void __fastcall btnShowAnotherClick(TObject *Sender);
	void __fastcall btnCancelClick(TObject *Sender);
	void __fastcall btnCancelAnotherClick(TObject *Sender);
	void __fastcall btnCancelAllClick(TObject *Sender);
	void __fastcall NotificationCenter1ReceiveLocalNotification(TObject *Sender, TNotification *ANotification);
private:	// User declarations
public:		// User declarations
	__fastcall TNotificationsApp(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TNotificationsApp *NotificationsApp;
//---------------------------------------------------------------------------
#endif
