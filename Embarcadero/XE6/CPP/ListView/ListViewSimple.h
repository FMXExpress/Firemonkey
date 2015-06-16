//---------------------------------------------------------------------------

#ifndef ListViewSimpleH
#define ListViewSimpleH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.GenData.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TSimpleListViewDelete : public TForm
{
__published:	// IDE-managed Components
	TToolBar *TopToolbar;
	TLabel *TitleLabel;
	TSpeedButton *EditButton;
	TSpeedButton *DoneButton;
	TToolBar *BottomToolbar;
	TSpeedButton *DeleteButton;
	TListView *ListView1;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	TLinkListControlToField *LinkListControlToField1;
	void __fastcall DeleteButtonClick(TObject *Sender);
	void __fastcall DoneButtonClick(TObject *Sender);
	void __fastcall EditButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TSimpleListViewDelete(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSimpleListViewDelete *SimpleListViewDelete;
//---------------------------------------------------------------------------
#endif
