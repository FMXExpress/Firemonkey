//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
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
#include <FMX.TabControl.hpp>
//---------------------------------------------------------------------------

class TForm1;

class TServerConnectionTH : public TThread
{
	friend TForm1;
private:
	TBluetoothServerSocket * FServerSocket;
	TBluetoothSocket *FSocket;
	TBytes FData;
	void __fastcall TThreadMethod(void);
	void __fastcall TThreadMethodException(void);
protected:
	void __fastcall Execute(void);
	void __fastcall UpdateText();
public:
	__fastcall TServerConnectionTH(bool ACreateSuspended) : TThread(ACreateSuspended){}
	__fastcall ~TServerConnectionTH(void);
};



class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TButton *ButtonDiscover;
	TButton *ButtonPair;
	TButton *ButtonPairedDevices;
	TButton *ButtonUnPair;
	TComboBox *ComboBoxDevices;
	TComboBox *ComboBoxPaired;
	TButton *ButtonServices;
	TComboBox *ComboBoxServices;
	TTabItem *TabItem2;
	TPanel *PanelClient;
	TButton *Button2;
	TEdit *Edit1;
	TButton *FreeSocket;
	TLabel *LabelNameServer;
	TLabel *LabelClient;
	TButton *ButtonConnectToRFCOMM;
	TPanel *PanelServer;
	TButton *ButtonCloseReadingSocket;
	TButton *ButtonOpenReadingSocket;
	TLabel *LabelServer;
	TLabel *Labeldiscoverable;
	TMemo *DisplayR;
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall ButtonCloseReadingSocketClick(TObject *Sender);
	void __fastcall ButtonConnectToRFCOMMClick(TObject *Sender);
	void __fastcall ButtonDiscoverClick(TObject *Sender);
	void __fastcall DevicesDiscoveryEnd(System::TObject* const Sender,
		TBluetoothDeviceList* const ADeviceList);
	void __fastcall ButtonOpenReadingSocketClick(TObject *Sender);
	void __fastcall ButtonPairClick(TObject *Sender);
	void __fastcall ButtonPairedDevicesClick(TObject *Sender);
	void __fastcall ButtonUnPairClick(TObject *Sender);
	void __fastcall FreeSocketClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall ButtonServicesClick(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall ComboBoxPairedChange(TObject *Sender);
private:	// User declarations
	TBluetoothManager * FBluetoothManager;
	TBluetoothDeviceList *FDiscoverDevices;
	TBluetoothDeviceList * FPairedDevices;
	TBluetoothAdapter * FAdapter;
	TBytes FData;
	TBluetoothSocket * FSocket;
	int ItemIndex;
	TServerConnectionTH * ServerConnectionTH;
	void __fastcall PairedDevices(void);
	void __fastcall SendData(void);
	String __fastcall GetServiceName(String GUID);
	bool __fastcall ManagerConnected(void);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
