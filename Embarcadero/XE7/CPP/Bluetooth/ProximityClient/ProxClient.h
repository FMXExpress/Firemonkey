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
//---------------------------------------------------------------------------
enum Position_t {poNear, poFar, poSoFar, poUnknown};

class TfrmProximityForm : public TForm
{
__published:	// IDE-managed Components
	TPanel *pnlLog;
	TMemo *Memo1;
	TPanel *pnlMain;
	TButton *btnScan;
	TLabel *lblDevice;
	TLabel *lblTxPower;
	TLabel *lblLinkLossAlert;
	TLabel *lblImmediateAlert;
	TEdit *Edit1;
	TLabel *lblRSSI;
	TLabel *lblDistance;
	TLabel *lblDist2;
	TLabel *lblPosition;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TButton *btnConnect;
	TComboBox *cbbDevice;
	TSplitter *Splitter1;
	TTimer *tmrReadRSSI;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall btnConnectClick(TObject *Sender);
	void __fastcall btnScanClick(TObject *Sender);
	void __fastcall tmrReadRSSITimer(TObject *Sender);
private:	// User declarations
	TBluetoothLEManager *FBLEManager;
	TBluetoothLEDevice *FBLEDevice;
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
	void __fastcall DoCharacteristicRead(TObject *Sender, TBluetoothGattCharacteristic *ACharacteristic,
										TBluetoothGattStatus *AGattStatus);
	void __fastcall DoCharacteristicWrite(TObject *Sender,
										  TBluetoothGattCharacteristic *ACharacteristic,
										  TBluetoothGattStatus *AGattStatus);
	void __fastcall DoDiscoveryEndEvent(TObject *Sender, TBluetoothLEDeviceList *ADeviceList);
	void __fastcall DoScan();
	void __fastcall DoReadRSSI(TObject *Sender, int ARssiValue, TBluetoothGattStatus *AGattStatus);
	void __fastcall DoServicesDiscovered(TObject *Sender, TBluetoothGattServiceList *AServiceList);
	void __fastcall EnableRSSIMonitorize(bool Enabled);
	void __fastcall GetServiceAndCharacteristics();
	void __fastcall SetPosition(Position_t Position);
	void __fastcall UpdateCurrentPosition(Position_t Position);
	void __fastcall WriteImmediateAlertLevel(Byte AlertLevel);
	void __fastcall WriteLinkLossAlertLevel(Byte AlertLevel);
public:		// User declarations
	__fastcall TfrmProximityForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmProximityForm *frmProximityForm;
//---------------------------------------------------------------------------
#endif
