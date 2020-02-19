//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ListViewCheckListFormH
#define ListViewCheckListFormH

#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Controls.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Classes.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TListView *ListView1;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	void __fastcall ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem);
	void __fastcall ListView1UpdateObjects(const TObject *Sender, const TListViewItem *AItem);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
