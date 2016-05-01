//---------------------------------------------------------------------------

#ifndef UMainH
#define UMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.Comp.UI.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.FMXUI.Wait.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.IB.hpp>
#include <FireDAC.Phys.IBBase.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FireDAC.Phys.IBDef.hpp>
//---------------------------------------------------------------------------
class TTIBLiteForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *AddButton;
	TButton *DeleteButton;
	TFDConnection *FireTaskList;
	TFDQuery *FDQueryInsert;
	TFDTable *FDTableTask;
	TFDQuery *FDQueryDelete;
	TFDGUIxWaitCursor *FDGUIxWaitCursor1;
	TFDPhysIBDriverLink *FDPhysIBDriverLink1;
	TWideStringField *FDTableTaskTASKNAME;
	TBindSourceDB *BindSourceDB1;
	TListView *ListViewTASKNAME;
	TLabel *LabelTASKNAME;
	TLinkFillControlToField *LinkFillControlToFieldTASKNAME;
	TBindingsList *BindingsList1;
	void __fastcall FireTaskListBeforeConnect(TObject *Sender);
	void __fastcall DeleteButtonClick(TObject *Sender);
	void __fastcall AddButtonClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	void __fastcall OnIdle(System::TObject* Sender, bool &Done);
	_di_TInputCloseQueryProc listQueryProc;
public:		// User declarations
	__fastcall TTIBLiteForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTIBLiteForm *TIBLiteForm;
//---------------------------------------------------------------------------
#endif
