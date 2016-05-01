//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Objects.hpp>
#include <FMX.SpinBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Beacon.hpp>
#include "BeaconsRender.h"
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
//---------------------------------------------------------------------------

#define TBeaconList System::DynamicArray<_di_IBeacon>

class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TStyleBook *StyleBook1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TPanel *Panel1;
	TScrollBox *ScrollBox2;
	TPanel *Panel6;
	TButton *BtnAddRegion;
	TButton *BtnDeleteRegion;
	TButton *btnStop;
	TButton *Button1;
	TButton *Button2;
	TComboBox *ComboBox1;
	TEdit *EdGuid;
	TLabel *Label1;
	TLabel *Label11;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TListBox *ListBox1;
	TSpinBox *sbMajor;
	TSpinBox *sbMinor;
	TTabItem *TabItem2;
	TTabControl *TabControl2;
	TTabItem *TabItem5;
	TListView *LvProximity;
	TTabItem *TabItem6;
	TListView *LvExitedRegion;
	TTabItem *TabItem7;
	TListView *LvExitedBeacon;
	TTabItem *TabItem8;
	TListView *LvEnteredBeacon;
	TTabItem *TabItem9;
	TListView *LvEnteredRegion;
	TTabItem *TabItem3;
	TPanel *Panel3;
	TListView *LvMonitoring;
	TTabItem *TabItem4;
	TRectangle *BeaconsRectangle;
	TPanel *Panel5;
	TLabel *Label10;
	TLabel *LbUUID;
	TLabel *LbMajor;
	TLabel *LbMinor;
	TLabel *Label14;
	TLabel *LbDistance;
	TLabel *Label16;
	TLabel *LbRssi;
	TSpinBox *SpinBox1;
	TTimer *Timer1;
	void __fastcall BtnAddRegionClick(TObject *Sender);
	void __fastcall BtnDeleteRegionClick(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall btnStopClick(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall BeaconsRectangleMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, float X, float Y);
	void __fastcall BeaconsRectanglePaint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall SpinBox1Change(TObject *Sender);
	void __fastcall sbMajorChange(TObject *Sender);
	void __fastcall ListBox1Click(TObject *Sender);
private:	// User declarations
	TBeaconManager *FBeaconManager;
	TObject *FLock;
	TBeaconList FCurrentBeaconList;
	TBeaconGraphicList FList;
	TRenderer *FRenderer;
	Double FMaxDistance;
	String FSelectedBeacon;
	void __fastcall CheckManager();
	void __fastcall BeaconEnter(System::TObject* const Sender, const _di_IBeacon ABeacon, const TBeaconList CurrentBeaconList);
	void __fastcall BeaconExit(System::TObject* const Sender, const _di_IBeacon ABeacon, const TBeaconList CurrentBeaconList);
	void __fastcall EnterRegion(System::TObject* const Sender, const GUID &UUID, int AMajor, int AMinor);
	void __fastcall ExitRegion(System::TObject* const Sender, const GUID &UUID, int AMajor, int AMinor);
	void __fastcall BeaconProximity(System::TObject* const Sender, const _di_IBeacon ABeacon, TBeaconProximity Proximity);
	void __fastcall StringToRegion(String AString, String &Guid,int &Major, int &Minor);

    void __fastcall BtnAddRegionCloseEvent(TObject *Sender,  const TModalResult AResult);
    void __fastcall AddRegion();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};

class TDummyInt : public TObject
{
public:
	Integer Number;
};

//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
