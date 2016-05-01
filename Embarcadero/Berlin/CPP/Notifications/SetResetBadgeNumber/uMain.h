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
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include "System.Notification.hpp"
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
