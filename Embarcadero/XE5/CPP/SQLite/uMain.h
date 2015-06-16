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
#include <Data.DbxSqlite.hpp>
#include <Data.FMTBcd.hpp>
#include <Data.SqlExpr.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>

//---------------------------------------------------------------------------
class TSQLiteForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnAdd;
	TButton *btnDelete;
	TListBox *ListBox1;
	TSQLConnection *TaskList;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	TSQLQuery *SQLQueryInsert;
	TSQLQuery *SQLQueryDelete;
	TSQLDataSet *SQLDataSetTask;
	TBindSourceDB *BindSourceDB1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall OnIdle(TObject * Sender, bool &ADone);
	void __fastcall btnAddClick(TObject *Sender);
	void __fastcall btnDeleteClick(TObject *Sender);
	void __fastcall TaskListBeforeConnect(TObject *Sender);
	void __fastcall TaskListAfterConnect(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TSQLiteForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSQLiteForm *SQLiteForm;
//---------------------------------------------------------------------------
#endif
