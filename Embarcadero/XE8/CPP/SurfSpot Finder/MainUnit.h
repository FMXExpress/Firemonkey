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
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.StorageBin.hpp>
#include <FMX.MultiView.hpp>
//---------------------------------------------------------------------------
class TForm5 : public TForm
{
__published:	// IDE-managed Components
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
	TBindSourceDB *BindSourceDB2;
	TLinkListControlToField *LinkListControlToField1;
	TLinkPropertyToField *LinkPropertyToFieldText4;
	TFDMemTable *FDMemTable1;
	TBindSourceDB *BindSourceDB1;
	TLinkPropertyToField *LinkPropertyToFieldText;
	TLinkPropertyToField *LinkPropertyToFieldText3;
	TLinkPropertyToField *LinkPropertyToFieldText2;
	TMultiView *MultiView1;
	TPanel *Panel1;
	void __fastcall ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall MultiViewToogle(TObject *Sender);
	void __fastcall MultiView1Paint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect);
	void __fastcall MultiView1Hidden(TObject *Sender);

private:	// User declarations
	void __fastcall UpdateWebBrowserVisibility();
public:		// User declarations
	__fastcall TForm5(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm5 *Form5;
//---------------------------------------------------------------------------
#endif
