//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ProxClientH
#define ProxClientH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
enum Position_t {poNear, poFar, poSoFar, poUnknown};

class TfrmProximityForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label5;
	TPanel *pnlLog;
	TMemo *Memo1;
	TPanel *pnlMain;
	TLabel *lblDevice;
	TLabel *lblPosition;
	TPanel *Panel1;
	TButton *btnScan;
	TPanel *Panel2;
	TLabel *Label4;
	TLabel *lblTxPower;
	TPanel *Panel3;
	TLabel *Label1;
	TLabel *lblRSSI;
	TLabel *lblLinkLossAlert;
	TLabel *lblImmediateAlert;
	TPanel *Panel4;
	TLabel *Label2;
	TLabel *lblDist2;
	TPanel *Panel5;
	TLabel *Label3;
	TLabel *lblDistance;
	TSplitter *Splitter1;
	TTimer *tmrReadRSSI;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall btnScanClick(TObject *Sender);
	void __fastcall tmrReadRSSITimer(TObject *Sender);
private:	// User declarations
	TBluetoothLEManager *FBLEManager;
	TBluetoothLEDevice *FBLEDevice;
	int FRatioDB;
	int FRssiValue;
	double FRatioLinear, FDistance;


	UnicodeString FLastMessage;
	bool FServicesDiscovered;
	int FTxPowerValue;

	TBluetoothGattService *FTXPowerService;
	TBluetoothGattService *FImmediateAlertService;
	TBluetoothGattService *FLinkLossService;

	TBluetoothGattCharacteristic *FTXPowerLevelCharact;
	TBluetoothGattCharacteristic *FImmediateAlertLevelCharact;
	TBluetoothGattCharacteristic *FLinkLossAlertLevelCharact;

	Position_t FCurrentPosition;
	int FNearCount;
	int FFarCount;
	int FSoFarCount;

	void __fastcall CheckDistanceThreshold(int PathLoss);
	void __fastcall OnDeviceDisconnect(TObject *Sender);
	void __fastcall DoCharacteristicRead(TObject *Sender, TBluetoothGattCharacteristic *ACharacteristic,
										TBluetoothGattStatus *AGattStatus);
	void __fastcall DoDiscoveryEndEvent(TObject *Sender, TBluetoothLEDeviceList *ADeviceList);
	void __fastcall CheckDeviceName();
	void __fastcall AddTextToMemo(UnicodeString S);
	void __fastcall SynchronizedMemoAdd();

	void __fastcall Connect();
	void __fastcall DoScan();
	void __fastcall DoReadRSSI(TObject *Sender, int ARssiValue, TBluetoothGattStatus AGattStatus);
	void __fastcall EnableRSSIMonitorize(bool Enabled);
	void __fastcall GetServiceAndCharacteristics();
	void __fastcall RefreshData();
	void __fastcall UpdatePositionLabel();
	void __fastcall SetPosition(Position_t Position);
	void __fastcall UpdateCurrentPosition(Position_t Position);
	void __fastcall WriteImmediateAlertLevel(System::Byte AlertLevel);
	void __fastcall WriteLinkLossAlertLevel(System::Byte AlertLevel);
public:		// User declarations
	__fastcall TfrmProximityForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmProximityForm *frmProximityForm;
//---------------------------------------------------------------------------
#endif
