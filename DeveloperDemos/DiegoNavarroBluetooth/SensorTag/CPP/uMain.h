//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.Components.hpp>
#include <System.Bluetooth.hpp>
//---------------------------------------------------------------------------
class TFrMainForm : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TListBox *ListBox1;
	TButton *Button2;
	TEdit *EdAmbient;
	TEdit *EdTarget;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TEdit *EdAccelX;
	TEdit *EdAccelY;
	TEdit *EdAccelZ;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TEdit *EdHumidity;
	TLabel *Label8;
	TBluetoothLE *BluetoothLE1;
	void __fastcall BluetoothLE1CharacteristicRead(TObject * const Sender, TBluetoothGattCharacteristic * const ACharacteristic,
          TBluetoothGattStatus AGattStatus);
	void __fastcall BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList);
	void __fastcall BluetoothLE1EndDiscoverServices(TObject * const Sender, TBluetoothGattServiceList * const AServiceList);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);



private:	// User declarations
	TBluetoothLEDevice *FcurrentDevice;
public:		// User declarations
	__fastcall TFrMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFrMainForm *FrMainForm;
//---------------------------------------------------------------------------
#endif
