//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.DB.hpp>
#include <Datasnap.DBClient.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.StdCtrls.hpp>
#include <Data.Bind.Controls.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TClientDataSet *ClientDataSet1;
	Data::Db::TDataSource *DataSource1;
	TBindingsList *BindingsList1;
	TBindSourceDB *BindSourceDB1;
	TEdit *EditCommon_Name;
	TLabel *Label1;
	TLinkControlToField *LinkControlToField1;
	TBindNavigator *NavigatorBindSourceDB1;
	TEdit *EditLength_In;
	TLabel *Label2;
	TLinkControlToField *LinkControlToField2;
	TMemo *MemoNotes;
	TLinkControlToField *LinkControlToField4;
	TImageControl *ImageControlGraphic;
	TLinkControlToField *LinkControlToField5;
private:	// User declarations
public:		// User declarations
	__fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
