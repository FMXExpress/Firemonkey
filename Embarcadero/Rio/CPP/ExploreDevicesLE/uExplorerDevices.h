//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uExplorerDevicesH
#define uExplorerDevicesH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TreeView.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.hpp>
//---------------------------------------------------------------------------
class TFrDeviceExplorer : public TForm
{
__published:	// IDE-managed Components
	TTimer *tmAnimateFindDevices;
	TScrollBox *ScrollBox1;
	TPanel *Panel2;
	TLabel *Label2;
	TCornerButton *CornerButton3;
	TProgressBar *PbServices;
	TTreeView *TvCharacteristics;
	TButton *btnGetValues;
	TPanel *Panel1;
	TLabel *Label4;
	TPanel *Panel4;
	TButton *Button1;
	TButton *Button2;
	TComboBox *CbDevices;
	TProgressBar *PbFindDevices;
	TPanel *Panel5;
	TLabel *Label1;
	TEdit *EdCurrentDevice;
	TPanel *Panel6;
	TScrollBox *ScrollBox2;
	TPanel *Panel8;
	TLabel *Label5;
	TLabel *Label8;
	TSpeedButton *btnRefresh;
	TButton *btnSuscribe;
	TButton *btnWrite;
	TEdit *EdCharacWrite;
	TComboBox *CbWriteTypes;
	TEdit *EdCharacName;
	TEdit *EdCharacUID;
	TLabel *Label7;
	TListBox *LbCurrentValue;
	TPanel *Panel7;
	TLabel *lbCurrentService;
	TPanel *Panel9;
	TLabel *Label6;
	TCheckBox *CbRead;
	TCheckBox *cbWriteNoResponse;
	TCheckBox *cbBroadcast;
	TCheckBox *CbWrite;
	TCheckBox *cbSignedWrite;
	TCheckBox *cbExtendedProp;
	TCheckBox *cbNotify;
	TCheckBox *cbIndicate;
	TPanel *Panel3;
	TTimer *tmAnimateFindServices;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tmAnimateFindDevicesTimer(TObject *Sender);
	void __fastcall tmAnimateFindServicesTimer(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall btnGetValuesClick(TObject *Sender);
	void __fastcall CornerButton3Click(TObject *Sender);
	void __fastcall TvCharacteristicsClick(TObject *Sender);
	void __fastcall btnRefreshClick(TObject *Sender);
	void __fastcall btnSuscribeClick(TObject *Sender);
	void __fastcall btnWriteClick(TObject *Sender);
	void __fastcall DevicesDiscoveryLEEnd(TObject* const Sender,
		TBluetoothLEDeviceList* const ADeviceList);
	void __fastcall DidCharacteristicRead(TObject* const Sender,
		TBluetoothGattCharacteristic* const ACharacteristic, TBluetoothGattStatus AGattStatus);
	void __fastcall ServicesDiscovered(TObject* const Sender,
		TBluetoothGattServiceList* const AServiceList);
private:	// User declarations
	TBluetoothGattCharacteristic * AChar;
	TTreeViewItem *ServiceItem;
	TTreeViewItem *Characteristic;
	TTreeViewItem *CharProps;
	int j;
	TBluetoothLEManager * FBluetoothManagerLE;
	int CurrentService;
	int CurrentCharacteristic;
	void __fastcall CleanDeviceInformation(void);
	UnicodeString __fastcall ConverToHex(TByteDynArray Bytes);
	void __fastcall RefreshCurrentCharacteristic(void);
	void __fastcall EmptyCharacteristic(void);
	TBluetoothLEDevice * __fastcall GetCurrentDevice();
public:		// User declarations
	__fastcall TFrDeviceExplorer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFrDeviceExplorer *FrDeviceExplorer;
//---------------------------------------------------------------------------
#endif
