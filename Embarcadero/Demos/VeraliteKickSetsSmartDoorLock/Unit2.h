//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <System.Actions.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <REST.Response.Adapter.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TActionList *ActionList1;
	TPreviousTabAction *PreviousTabAction;
	TNextTabAction *NextTabAction;
	TGestureManager *GestureManager1;
	TRESTClient *RESTClient1;
	TRESTRequest *RESTRequest1;
	TRESTResponse *RESTResponse1;
	TTabControl *TabControl1;
	TTabItem *TabItemLock;
	TImage *ImageLock;
	TTabItem *TabItemUnlock;
	TImage *ImageUnlock;
	TToolBar *ToolBarHeader;
	TLabel *LabelHeader;
	TRESTClient *RESTClient2;
	TRESTRequest *RESTRequest2;
	TRESTResponse *RESTResponse2;
	void __fastcall TabControl1Change(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
