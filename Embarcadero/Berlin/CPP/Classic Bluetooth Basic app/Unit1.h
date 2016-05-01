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
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.ScrollBox.hpp>
#include <System.ImageList.hpp>
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
	TImageList *ImageList1;
	TPanel *Panel1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TPanel *PnlDiscover;
	TButton *ButtonDiscover;
	TButton *ButtonPair;
	TComboBox *ComboBoxDevices;
	TAniIndicator *AniIndicator1;
	TPanel *PnlPairedDevs;
	TButton *ButtonPairedDevices;
	TButton *ButtonUnPair;
	TComboBox *ComboBoxPaired;
	TPanel *Panel2;
	TButton *ButtonServices;
	TAniIndicator *AniIndicator2;
	TListView *ListView1;
	TTabItem *TabItem2;
	TPanel *PanelClient;
	TButton *Button2;
	TEdit *Edit1;
	TButton *FreeSocket;
	TLabel *LabelNameServer;
	TLabel *LabelClient;
	TButton *ButtonConnectToRFCOMM;
	TPanel *PanelServer;
	TButton *ButtonOpenReadingSocket;
	TLabel *LabelServer;
	TMemo *DisplayR;
	TLabel *Labeldiscoverable;
	TStyleBook *StyleBook1;
	void __fastcall Button2Click(TObject *Sender);
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
	Integer __fastcall GetServiceImageIndex(TGUID AServiceUUID);
	void __fastcall AddDiscoveredDevice(void);

public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
