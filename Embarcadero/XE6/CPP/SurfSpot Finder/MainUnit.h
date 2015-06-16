//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.WebBrowser.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Data.DB.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <REST.Response.Adapter.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <Data.Bind.DBScope.hpp>
#include <FMX.ActnList.hpp>
#include <System.Actions.hpp>
#include <Datasnap.DBClient.hpp>
//---------------------------------------------------------------------------
class TForm5 : public TForm
{
__published:	// IDE-managed Components
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TTabItem *TabItem2;
	TToolBar *ToolBar1;
	TToolBar *ToolBar2;
	TLabel *SurfSpotLabel;
	TImage *SurferImage;
	TListView *ListView1;
	TLabel *SurfSpotNameLabel;
	TSpeedButton *MyBackButton;
	TListBox *LocationListBox;
	TListBoxItem *LatitudeItem;
	TListBoxItem *LongitudeItem;
	TListBoxItem *CountyItem;
	TWebBrowser *WebBrowser1;
	TRESTClient *RESTClient1;
	TRESTRequest *RESTRequest1;
	TRESTResponse *RESTResponse1;
	TRESTResponseDataSetAdapter *RESTResponseDataSetAdapter1;
	TBindingsList *BindingsList1;
	TActionList *ActionList1;
	TChangeTabAction *ChangeTabAction1;
	TChangeTabAction *ChangeTabAction2;
	TClientDataSet *ClientDataSet1;
	TBindSourceDB *BindSourceDB2;
	TLinkPropertyToField *LinkPropertyToFieldText;
	TLinkPropertyToField *LinkPropertyToFieldText2;
	TLinkPropertyToField *LinkPropertyToFieldText3;
	TLinkListControlToField *LinkListControlToField1;
	TLinkPropertyToField *LinkPropertyToFieldText4;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem);
	void __fastcall FormShow(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TForm5(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm5 *Form5;
//---------------------------------------------------------------------------
#endif
