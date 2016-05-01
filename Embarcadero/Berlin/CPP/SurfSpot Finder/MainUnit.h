//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

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
#include <Data.Bind.ObjectScope.hpp>
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
#include <FireDAC.Stan.StorageBin.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.MultiView.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.WebBrowser.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <REST.Response.Adapter.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TListBox *LocationListBox;
	TListBoxItem *LatitudeItem;
	TListBoxItem *LongitudeItem;
	TListBoxItem *CountyItem;
	TToolBar *ToolBar2;
	TLabel *SurfSpotNameLabel;
	TSpeedButton *MyBackButton;
	TWebBrowser *WebBrowser1;
	TMultiView *MultiView1;
	TListView *ListView1;
	TToolBar *ToolBar1;
	TLabel *SurfSpotLabel;
	TImage *SurferImage;
	TRESTResponseDataSetAdapter *RESTResponseDataSetAdapter1;
	TBindingsList *BindingsList1;
	TLinkListControlToField *LinkListControlToField1;
	TLinkPropertyToField *LinkPropertyToFieldText4;
	TLinkPropertyToField *LinkPropertyToFieldText;
	TLinkPropertyToField *LinkPropertyToFieldText3;
	TLinkPropertyToField *LinkPropertyToFieldText2;
	TBindSourceDB *BindSourceDB2;
	TFDMemTable *FDMemTable1;
	TBindSourceDB *BindSourceDB1;
	TRESTClient *RESTClient1;
	TRESTRequest *RESTRequest1;
	TRESTResponse *RESTResponse1;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall ListView1ItemClick(TObject * const Sender, TListViewItem * const AItem);
	void __fastcall MultiView1Paint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect);
	void __fastcall MultiView1Hidden(TObject *Sender);


private:	// User declarations
	void __fastcall UpdateWebBrowserVisibility();
public:		// User declarations
	__fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
