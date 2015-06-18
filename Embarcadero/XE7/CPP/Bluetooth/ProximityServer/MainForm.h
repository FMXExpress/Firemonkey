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
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TButton *StartAnnounce;
	TImageControl *ImageControl1;
	TLabel *Label1;
	TLabel *lbDeviceConnected;
	TMemo *Memo1;
	TRectangle *Rectangle1;
	TColorAnimation *highAlertAnimation;
	TColorAnimation *medAlertAnimation;
	TSwitch *swConnected;
	TTimer *tmrReviewDeviceConnected;
	TTimer *tmrRSSI;
	TText *txtALERT;
	void __fastcall StartAnnounceClick(TObject *Sender);
	void __fastcall tmrRSSITimer(TObject *Sender);
	void __fastcall tmrReviewDeviceConnectedTimer(TObject *Sender);
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
