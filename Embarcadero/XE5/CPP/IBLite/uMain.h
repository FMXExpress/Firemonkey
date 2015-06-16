//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.DB.hpp>
#include <Data.DBXInterbase.hpp>
#include <Data.FMTBcd.hpp>
#include <Data.SqlExpr.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TIBLiteForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *AddButton;
	TButton *DeleteButton;
	TListView *ListView1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	TBindSourceDB *BindSourceDB1;
	TSQLQuery *SQLQueryDelete;
	TSQLQuery *SQLQueryInsert;
	TSQLDataSet *SQLDataSetTask;
	TSQLConnection *TaskList;
	void __fastcall OnIdle(TObject* Sender, bool &Done);
	void __fastcall AddButtonClick(TObject *Sender);
	void __fastcall DeleteButtonClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall TaskListBeforeConnect(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TIBLiteForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TIBLiteForm *IBLiteForm;
//---------------------------------------------------------------------------
#endif
