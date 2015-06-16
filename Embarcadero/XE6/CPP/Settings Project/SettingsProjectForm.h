//---------------------------------------------------------------------------

#ifndef SettingsProjectFormH
#define SettingsProjectFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <FMX.ActnList.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TSettingsForm : public TForm
{
__published:	// IDE-managed Components
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TToolBar *ToolBar1;
	TLabel *SettingsMain;
	TListBox *SettingsList1;
	TListBoxGroupHeader *AccountInfo;
	TListBoxItem *AccountType;
	TListBoxItem *PaymentType;
	TListBoxItem *RenewalType;
	TListBoxGroupHeader *SyncSettings;
	TListBoxItem *SyncUSB;
	TSwitch *Switch1;
	TListBoxItem *SyncWifi;
	TSwitch *Switch2;
	TListBoxItem *SyncCollections;
	TSwitch *Switch3;
	TTabItem *TabItem2;
	TToolBar *ToolBar2;
	TLabel *SettingsDetails;
	TSpeedButton *BackButton;
	TListBox *SettingsList2;
	TListBoxGroupHeader *AcctTypes;
	TListBoxItem *SelectAcctType;
	TComboBox *AcctCombo;
	TListBoxItem *Business;
	TListBoxItem *Personal;
	TListBoxItem *SelectPayment;
	TComboBox *PaymentCombo;
	TListBoxItem *CreditCard;
	TListBoxItem *Check;
	TListBoxItem *SelectRenewal;
	TComboBox *RenewalCombo;
	TListBoxItem *Monthly;
	TListBoxItem *Annually;
	TListBoxItem *Quarterly;
	TBindingsList *BindingsList1;
	TLinkFillControlToProperty *LinkFillControlToPropertyItemDataDetail;
	TLinkFillControlToProperty *LinkFillControlToPropertyItemDataDetail2;
	TLinkFillControlToProperty *LinkFillControlToPropertyItemDataDetail3;
	TActionList *ActionList1;
	TChangeTabAction *ChangeTabAction1;
	TChangeTabAction *ChangeTabAction2;
	void __fastcall ListBoxItemTab1Click(TObject * Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TSettingsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSettingsForm *SettingsForm;
//---------------------------------------------------------------------------
#endif
