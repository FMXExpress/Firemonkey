//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include "FMX.PhoneDialer.hpp"
#include <FMX.MobilePreview.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TPhoneDialerForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnGetCarrierInfo;
	TButton *btnMakeCall;
	TEdit *edtTelephoneNumber;
	TLabel *lblTelephoneNumber;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *ListBox1;
	TListBoxItem *CarrierNameItem;
	TListBoxItem *CountryCodeItem;
	TListBoxItem *NetworkCodeItem;
	TListBoxItem *MobileNetworkItem;
	void __fastcall btnGetCarrierInfoClick(TObject *Sender);
	void __fastcall btnMakeCallClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TPhoneDialerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPhoneDialerForm *PhoneDialerForm;
//---------------------------------------------------------------------------
#endif
