//---------------------------------------------------------------------------

#ifndef LocationDemoUnitH
#define LocationDemoUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Objects.hpp>
#include <System.Sensors.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.WebBrowser.hpp>
#include <System.Sensors.hpp>
#include <FMX.MobilePreview.hpp>
#include <System.Sensors.Components.hpp>
#include <FMX.MultiView.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TLocationSensor *LocationSensor1;
	TWebBrowser *WebBrowser1;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TSwitch *Switch1;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *ListBoxItemLatitude;
	TListBoxItem *ListBoxItemLongitude;
	TListBoxGroupHeader *ListBoxGroupHeader2;
	TListBoxItem *ListBoxItemAdminArea;
	TListBoxItem *ListBoxItemCountryCode;
	TListBoxItem *ListBoxItemCountryName;
	TListBoxItem *ListBoxItemFeatureName;
	TListBoxItem *ListBoxItemLocality;
	TListBoxItem *ListBoxItemPostalCode;
	TListBoxItem *ListBoxItemSubAdminArea;
	TListBoxItem *ListBoxItemSubLocality;
	TListBoxItem *ListBoxItemSubThoroughfare;
	TListBoxItem *ListBoxItemThoroughfare;
	TLabel *Label1;
	TMultiView *MultiView1;
	TLayout *Layout1;
	TToolBar *ToolBar1;
	TToolBar *ToolBar2;
	TButton *Button2;
	TButton *Button1;
	void __fastcall Switch1Switch(TObject *Sender);
	void __fastcall LocationSensor1LocationChanged(TObject *Sender, const TLocationCoord2D &OldLocation,
          const TLocationCoord2D &NewLocation);
	void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
	TGeocoder * FGeocoder;
	void __fastcall OnGeocodeReverseEvent(TCivicAddress* const Address);
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
