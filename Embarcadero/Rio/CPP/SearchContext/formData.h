//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef formDataH
#define formDataH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <Data.DB.hpp>
//---------------------------------------------------------------------------
class TfrmData : public TForm
{
__published:	// IDE-managed Components
	TImageControl *ImageControl1;
	TLabel *Label3;
	TEdit *Edit2;
	TLabel *Label2;
	TEdit *Edit1;
	TLabel *Label1;
	TLabel *Label4;
	TMemo *Memo1;
	TOpenDialog *OpenDialog1;
	TBindSourceDB *BindSourceDB1;
	TBindingsList *BindingsList1;
	TLinkControlToField *LinkControlToField1;
	TLinkControlToField *LinkControlToField2;
	TLinkControlToField *LinkControlToField3;
	TLinkControlToField *LinkControlToField4;
	TBindNavigator *NavigatorBindSourceDB1;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmData(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmData *frmData;
//---------------------------------------------------------------------------
#endif
