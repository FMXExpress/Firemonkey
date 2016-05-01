//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include "FMX.Notification.hpp"
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.NumberBox.hpp>
//---------------------------------------------------------------------------
class TTSettingBadgeNumberForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnSetBadgeNumber;
	TButton *btnResetBadgeNumber;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TNumberBox *nbBadgeNumber;
	TButton *btnBadgeNumberDown;
	TButton *btnBadgeNumberUp;
	TNotificationCenter *NotificationC;
	void __fastcall btnSetBadgeNumberClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall btnResetBadgeNumberClick(TObject *Sender);
	void __fastcall btnBadgeNumberDownClick(TObject *Sender);
	void __fastcall btnBadgeNumberUpClick(TObject *Sender);
private:	// User declarations
	Single __fastcall GetBadgeNumber();
public:		// User declarations
	__fastcall TTSettingBadgeNumberForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTSettingBadgeNumberForm *TSettingBadgeNumberForm;
//---------------------------------------------------------------------------
#endif
