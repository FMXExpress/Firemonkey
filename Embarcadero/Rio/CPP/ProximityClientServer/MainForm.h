//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <System.Bluetooth.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TLabel *Label2;
	TMemo *Memo1;
	TPanel *Panel1;
	TLabel *lbDeviceConnected;
	TImageControl *ImageControl1;
	TButton *BtnStartAnnounce;
	TSwitch *swConnected;
	TRectangle *Rectangle1;
	TColorAnimation *highAlertAnimation;
	TColorAnimation *medAlertAnimation;
	TLabel *lbALERT;
	void __fastcall StartAnnounceClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	TBluetoothLEDevice *FBluetoothLEDevice;
	TBluetoothLEManager *FBluetoothManagerLE;
	TBluetoothGattServer *FGattServer;
	TBluetoothGattService *FGenericAccessService;

	TBluetoothGattService *FLinkLossService;
	TBluetoothGattService *FImmediateAlertService;
	TBluetoothGattService *FTxPowerService;

	TBluetoothGattCharacteristic *FDeviceNameCharact;

	TBluetoothGattCharacteristic *FLinkLoss;
	TBluetoothGattCharacteristic *FImmediateAlert;
	TBluetoothGattCharacteristic *FTxPower;

	TBluetoothGattCharacteristic *AChar;
	Integer IByte;

	void __fastcall DoOnConnectedDevice(TObject *Sender, TBluetoothLEDevice *ADevice);
	void __fastcall MyReadEvent(TObject *Sender, TBluetoothGattCharacteristic *ACharacteristic,
								TBluetoothGattStatus *AGattStatus);
	void __fastcall MyWriteEvent(TObject *Sender, TBluetoothGattCharacteristic *ACharacteristic,
								TBluetoothGattStatus *AGattStatus, TBytes AValue);
	void __fastcall UpdateTextAndSwitch();
	String __fastcall BytesToString(TBytes B);
public:		// User declarations
	__fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
