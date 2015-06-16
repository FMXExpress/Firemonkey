//---------------------------------------------------------------------------

#ifndef MainFrm_PhoneH
#define MainFrm_PhoneH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "MainFrm.h"
#include <FMX.ActnList.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
//---------------------------------------------------------------------------
class TPhoneMainForm : public TBaseMainForm
{
__published:	// IDE-managed Components
	TText *TextToSelectImage;
	void __fastcall ActionListUpdate(TBasicAction *Action, bool &Handled);
	void __fastcall ImageContainerClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TPhoneMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPhoneMainForm *PhoneMainForm;
//---------------------------------------------------------------------------
#endif
