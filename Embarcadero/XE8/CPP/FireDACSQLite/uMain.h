//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.DB.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Classes.hpp>
#include <System.Rtti.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.Comp.UI.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.FMXUI.Wait.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Phys.SQLite.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.ExprFuncs.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
class TFireDAC_SQLiteForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TButton *btnAdd;
	TLabel *Title;
	TButton *btnDelete;
	TListBox *ListBox1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	TBindSourceDB *BindSourceDB1;
	TFDTable *FDTableTask;
	TFDConnection *FireTaskList;
	TFDQuery *FDQueryDelete;
	TFDQuery *FDQueryInsert;
	TFDPhysSQLiteDriverLink *FDPhysSQLiteDriverLink1;
	TFDGUIxWaitCursor *FDGUIxWaitCursor1;
	void __fastcall btnAddClick(TObject *Sender);
	void __fastcall btnDeleteClick(TObject *Sender);
	void __fastcall OnIdle(TObject* Sender, bool &Done);
	void __fastcall FireTaskListAfterConnect(TObject *Sender);
	void __fastcall FireTaskListBeforeConnect(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFireDAC_SQLiteForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFireDAC_SQLiteForm *FireDAC_SQLiteForm;
//---------------------------------------------------------------------------
#endif
