//---------------------------------------------------------------------------

#ifndef VacationUnitH
#define VacationUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Data.DB.hpp>
#include <Datasnap.DBClient.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <REST.Response.Adapter.hpp>
#include <System.Actions.hpp>
#include <FMX.Edit.hpp>
#include <FMX.DateTimeCtrls.hpp>
#include <FMX.WebBrowser.hpp>
#include <System.Sensors.hpp>
//---------------------------------------------------------------------------
class TForm6 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TSpeedButton *RefreshButton;
	TTabControl *VacationTabControl;
	TTabItem *WeatherTabItem;
	TTabItem *SettingsTabItem;
	TListBox *WeatherListBox;
	TClientDataSet *ClientDataSet1;
	TRESTResponseDataSetAdapter *RESTResponseDataSetAdapter1;
	TActionList *ActionList1;
	TAction *MyAction;
	TRESTClient *RESTClient1;
	TRESTRequest *RESTRequest1;
	TRESTResponse *RESTResponse1;
	TListBox *ListBox1;
	TListBoxItem *LocationListBoxItem;
	TEdit *AddressEdit;
	TListBoxItem *DateListBoxItem;
	TCalendarEdit *CalendarEdit1;
	TListBoxItem *LatLongListBoxItem;
	TTabItem *BrowserTabItem;
	TLabel *CountdownLabel;
	TWebBrowser *WebBrowser1;
	TLabel *LatLongLabel;
	void __fastcall MyActionExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall AddressEditChange(TObject *Sender);
private:	// User declarations
	System::Boolean FoundAddrLatLong;  // if LatLong found using Address
	double FoundLatitude;
	double FoundLongitude;
	TCivicAddress *lAddress;
	String APIKeyString;
	TGeocoder * FGeocoder;
	// _dt_System_Sensors_1
	// void __fastcall OnGeocodeEvent(const TLocationCoord2D Coords);
	void __fastcall OnGeocodeEvent(System::DynamicArray<TLocationCoord2D> Coords);
public:		// User declarations
	__fastcall TForm6(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm6 *Form6;
//---------------------------------------------------------------------------
#endif
