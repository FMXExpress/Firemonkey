//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Notification.hpp>
#include <FMX.MobilePreview.hpp>
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
	void __fastcall btnSendScheduledNotificationClick(TObject *Sender);
	void __fastcall btnSendNotificationImmediatelyClick(TObject *Sender);
	void __fastcall SpeedButton1Click(TObject *Sender);
	void __fastcall SpeedButton2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TNotificationsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TNotificationsForm *NotificationsForm;
//---------------------------------------------------------------------------
#endif
