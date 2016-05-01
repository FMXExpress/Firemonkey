//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.hpp>
#include <FMX.Edit.hpp>
#include <list>
//---------------------------------------------------------------------------

  //TSCANRESPONSE POSITIONS
const  Integer BEACON_TYPE_POSITION = 2;
const  Integer BEACON_GUID_POSITION = 4;
const  Integer BEACON_MAJOR_POSITION = 20;
const  Integer BEACON_MINOR_POSITION = 22;
const  Word BEACON_ST_TYPE = 0x0215;

  struct TBeaconDevice{
    TBluetoothLEDevice* ADevice;
	TGUID GUID;
	Word Major;
	Word Minor;
	Integer TxPower;
	Integer Rssi;
	Double Distance;
	System::Boolean Alt;
  };
  typedef std::list<TBeaconDevice> TBeaconDeviceList;

class MyThreadProcedure : public TCppInterfacedObject<TThreadProcedure>
{
public:
	MyThreadProcedure(TBluetoothLEDevice* const _ADevice, int _Rssi, TScanResponse* const _ScanResponse);
	void __fastcall Invoke(void);
private:
	TBluetoothLEDevice* const ADevice;
	int Rssi;
	TScanResponse* const ScanResponse;
	bool __fastcall DecodeScanResponse(TScanResponse* const ScanResponse, TBeaconDevice &BeaconDevice);
};

class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TButton *Button1;
	TButton *Button2;
	TListBox *ListBox1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
  TBluetoothLEManager* FManager;
  TBeaconDeviceList BeaconDeviceList;

  void __fastcall DiscoverLEDevice(System::TObject* const Sender, TBluetoothLEDevice* const ADevice, int Rssi, TScanResponse* const ScanResponse);


public:		// User declarations
	__fastcall TForm2(TComponent* Owner);

	friend class MyThreadProcedure;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
