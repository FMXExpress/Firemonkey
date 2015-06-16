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
//---------------------------------------------------------------------------
class TPhoneDialerForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnGetCarrierInfo;
	TLabel *lblCarrierName;
	TLabel *lblISOCountryCode;
	TLabel *lblNetworkCode;
	TLabel *lblMobileNetwork;
	TButton *btnMakeCall;
	TEdit *edtTelephoneNumber;
	TLabel *lblTelephoneNumber;
	TToolBar *ToolBar1;
	TLabel *Label1;
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
