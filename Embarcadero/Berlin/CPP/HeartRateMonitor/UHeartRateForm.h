//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef UHeartRateFormH
#define UHeartRateFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ScrollBox.hpp>
#include <System.Bluetooth.Components.hpp>

//---------------------------------------------------------------------------
enum TSensorContactStatus {NonSupported, NonDetected, Detected};


struct THRMFlags {
	bool HRValue16bits;
	TSensorContactStatus SensorContactStatus;
	bool EnergyExpended;
	bool RRInterval;
};


class TfrmHeartMonitor : public TForm
{
__published:	// IDE-managed Components
	TPanel *pnlMain;
	TButton *btnScan;
	TLabel *lblDevice;
	TLabel *lblBPM;
	TLabel *lblBodyLocation;
	TLabel *lblContactStatus;
	TImage *imgHeart;
	TButton *btnMonitorize;
	TPanel *pnlLog;
	TListBox *LogList;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *ListBoxItem1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *MainList;
	TListBoxItem *DeviceScan;
	TListBoxItem *BPM;
	TListBoxItem *Image;
	TListBoxItem *Location;
	TListBoxItem *Status;
	TListBoxItem *Monitoring;
	TMemo *Memo1;
	TBluetoothLE *BluetoothLE1;
	void __fastcall btnMonitorizeClick(TObject *Sender);
	void __fastcall btnScanClick(TObject *Sender);
	void __fastcall BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList);
	void __fastcall BluetoothLE1DescriptorRead(TObject * const Sender, TBluetoothGattDescriptor * const ADescriptor, TBluetoothGattStatus AGattStatus);
	void __fastcall BluetoothLE1CharacteristicRead(TObject * const Sender, TBluetoothGattCharacteristic * const ACharacteristic,
          TBluetoothGattStatus AGattStatus);
private:	// User declarations
	TBluetoothLEDevice * FBLEDevice;
	bool FServicesDiscovered;

	TBluetoothGattService * FHRGattService;
	TBluetoothGattCharacteristic * FHRMeasurementGattCharact;
	TBluetoothGattCharacteristic *FBodySensorLocationGattCharact;

	void __fastcall DoDiscoveryEndEvent(System::TObject* const Sender, TBluetoothLEDeviceList* const ADeviceList);
	void __fastcall DoServicesDiscovered(System::TObject* const Sender, TBluetoothGattServiceList* const AServiceList);
	void __fastcall DoCharacteristicRead(System::TObject* const Sender, TBluetoothGattCharacteristic* const ACharacteristic, TBluetoothGattStatus AGattStatus);
	void __fastcall DoDescriptorRead(System::TObject* const Sender, TBluetoothGattDescriptor* const ADescriptor, TBluetoothGattStatus AGattStatus);

	void __fastcall GetServiceAndCharacteristics(void);

	void __fastcall ManageCharacteristicData(const TBluetoothGattCharacteristic *ACharacteristic);
	void __fastcall DisplayHeartRateMeasurementData(TBytes Data);
	void __fastcall DisplayBodySensorLocationData(UInt8 Index);

	THRMFlags * _fastcall GetFlags(System::Byte Data);
	void __fastcall EnableHRMMonitorize(bool Enabled);
	void __fastcall ReadBodySensorLocation(void);

	void __fastcall ClearData(void);
	void __fastcall DoScan(void);

public:		// User declarations
	__fastcall TfrmHeartMonitor(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmHeartMonitor *frmHeartMonitor;
//---------------------------------------------------------------------------
#endif
