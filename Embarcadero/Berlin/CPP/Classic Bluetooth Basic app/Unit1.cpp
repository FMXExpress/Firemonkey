//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
#include <vector>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;

String Msg = "";
const String ServiceName = _D("Basic Text Server");
const String ServiceGUI = _D("{B62C4E8D-62CC-404B-BBBF-BF3E3BBB1378}");

struct ServiceImg
{
	String Name;
	TBluetoothUUID UUID;
	int ImageIndex;

	ServiceImg(String nam, TBluetoothUUID TBUUID, int Im): Name(nam), UUID(TBUUID), ImageIndex(Im)
	{}
};
std::vector<ServiceImg> ServiceImages;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
	ServiceImages.push_back(ServiceImg("LAN Access Using PPP", StringToGUID("{00001102-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("DialupNetworking", StringToGUID("{00001103-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("OBEXObjectPush", StringToGUID("{00001105-0000-1000-8000-00805F9B34FB}"), 8));
	ServiceImages.push_back(ServiceImg("OBEXFileTransfer", StringToGUID("{00001106-0000-1000-8000-00805F9B34FB}"), 8));
	ServiceImages.push_back(ServiceImg("Cordless Telephony", StringToGUID("{00001109-0000-1000-8000-00805F9B34FB}"), 5));
	ServiceImages.push_back(ServiceImg("Audio Source", StringToGUID("{0000110A-0000-1000-8000-00805F9B34FB}"), 1));
	ServiceImages.push_back(ServiceImg("Audio Sink", StringToGUID("{0000110B-0000-1000-8000-00805F9B34FB}"), 1));
	ServiceImages.push_back(ServiceImg("AV Remote Control Target", StringToGUID("{0000110C-0000-1000-8000-00805F9B34FB}"), 2));
	ServiceImages.push_back(ServiceImg("Advanced Audio Distribution", StringToGUID("{0000110D-0000-1000-8000-00805F9B34FB}"), 1));
	ServiceImages.push_back(ServiceImg("AV Remote Control", StringToGUID("{0000110E-0000-1000-8000-00805F9B34FB}"), 2));
	ServiceImages.push_back(ServiceImg("Headset Audio Gateway", StringToGUID("{00001112-0000-1000-8000-00805F9B34FB}"), 6));
	ServiceImages.push_back(ServiceImg("WAP", StringToGUID("{00001113-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("WAP Client", StringToGUID("{00001114-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("Personal Area Network User (PANU)", StringToGUID("{00001115-0000-1000-8000-00805F9B34FB}"), 9));
	ServiceImages.push_back(ServiceImg("Network Access Point (NAP)", StringToGUID("{00001116-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("Group Ad-hoc Network (GN)", StringToGUID("{00001117-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("Handsfree", StringToGUID("{0000111E-0000-1000-8000-00805F9B34FB}"), 5));
	ServiceImages.push_back(ServiceImg("Handsfree Audio Gateway", StringToGUID("{0000111F-0000-1000-8000-00805F9B34FB}"), 5));
	ServiceImages.push_back(ServiceImg("SIM Access", StringToGUID("{0000112D-0000-1000-8000-00805F9B34FB}"), 10));
	ServiceImages.push_back(ServiceImg("Phonebook Access - PCE", StringToGUID("{0000112E-0000-1000-8000-00805F9B34FB}"), 0));
	ServiceImages.push_back(ServiceImg("Phonebook Access - PSE", StringToGUID("{0000112F-0000-1000-8000-00805F9B34FB}"), 0));
	ServiceImages.push_back(ServiceImg("Phonebook Access", StringToGUID("{00001130-0000-1000-8000-00805F9B34FB}"), 0));
	ServiceImages.push_back(ServiceImg("Headset headset", StringToGUID("{00001131-0000-1000-8000-00805F9B34FB}"), 6));
	ServiceImages.push_back(ServiceImg("Message Access Server", StringToGUID("{00001132-0000-1000-8000-00805F9B34FB}"), 4));
	ServiceImages.push_back(ServiceImg("Message Notification Server", StringToGUID("{00001133-0000-1000-8000-00805F9B34FB}"), 4));
	ServiceImages.push_back(ServiceImg("Message Access Profile", StringToGUID("{00001134-0000-1000-8000-00805F9B34FB}"), 4));
	ServiceImages.push_back(ServiceImg("Generic Networking", StringToGUID("{00001201-0000-1000-8000-00805F9B34FB}"), 7));
	ServiceImages.push_back(ServiceImg("Generic Audio", StringToGUID("{00001203-0000-1000-8000-00805F9B34FB}"), 1));
}
//---------------------------------------------------------------------------
void __fastcall TServerConnectionTH::TThreadMethodException(void)
{
	Form1->DisplayR->Lines->Add("Server socket closed: " + Msg);
}
//---------------------------------------------------------------------------
void __fastcall TServerConnectionTH::TThreadMethod(void)
{
	Form1->DisplayR->Lines->Add(TEncoding::UTF8->GetString(FData));
}
//---------------------------------------------------------------------------
void __fastcall TServerConnectionTH::Execute(void)
{
	ReturnValue = -1;
	while(!Terminated) {
		try {
			TBluetoothSocket * ASocket = NULL;
			while((!Terminated) && (ASocket == NULL)) {
				ASocket = FServerSocket->Accept(100);
			}
			ReturnValue = 0;
			if(ASocket != NULL) {
				FSocket = ASocket;
				while (!Terminated){
					FData = ASocket->ReadData();
					if(FData.Length > 0) {
						Synchronize(TThreadMethod);
					}
					Sleep(100);
				}
			}
		} catch (Exception &ex) {
			Msg = ex.Message;
			Synchronize(TThreadMethodException);
		}
    }
}
//---------------------------------------------------------------------------
void __fastcall TServerConnectionTH::UpdateText()
{
	if(FData.Length > 0) {
        Form1->DisplayR->Lines->Add(TEncoding::UTF8->GetString(FData));
    }
}
//---------------------------------------------------------------------------
__fastcall TServerConnectionTH::~TServerConnectionTH(void)
{
	FSocket->Free();
	FServerSocket->Free();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PairedDevices(void)
{
	ComboBoxPaired->Clear();
	FPairedDevices = FBluetoothManager->GetPairedDevices();
	if(FPairedDevices->Count > 0) {
		for(int i = 0; i < FPairedDevices->Count; i++) {
			ComboBoxPaired->Items->Add(FPairedDevices->Items[i]->DeviceName);
        }
	}
	else {
		ComboBoxPaired->Items->Add("No Paired Devices");
    }
}
//---------------------------------------------------------------------------
String __fastcall TForm1::GetServiceName(String GUID)
{
	String _return = "";
	TBluetoothDevice * ADevice = FPairedDevices->Items[ComboBoxPaired->ItemIndex];
	TBluetoothServiceList * LServices = ADevice->GetServices();
	for(int i = 0; i < LServices->Count; i++)
	{
		if(StringToGUID(GUID) == LServices->Items[i].UUID) {
			_return = LServices->Items[i].Name;
			break;
		}
	}
	return _return;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SendData(void)
{
	TBytes ToSend;
	if((FSocket == NULL) || (ItemIndex != ComboBoxPaired->ItemIndex)) {
		if(ComboBoxPaired->ItemIndex > -1) {
			TBluetoothDevice * LDevice = FPairedDevices->Items[ComboBoxPaired->ItemIndex];
			DisplayR->Lines->Add(GetServiceName(ServiceGUI));
			DisplayR->GoToTextEnd();
			FSocket = LDevice->CreateClientSocket(StringToGUID(ServiceGUI), false);
			if(FSocket != NULL) {
				ItemIndex = ComboBoxPaired->ItemIndex;
				FSocket->Connect();
				ToSend = TEncoding::UTF8->GetBytes(Edit1->Text);
				FSocket->SendData(ToSend);
				DisplayR->Lines->Add("Text Sent");
				DisplayR->GoToTextEnd();
			}
			else {
				ShowMessage("Out of time ~15s~");
			}
		}
		else {
			ShowMessage("No paired device selected");
		}
	}
	else {
		ToSend = TEncoding::UTF8->GetBytes(Edit1->Text);
		FSocket->SendData(ToSend);
		DisplayR->Lines->Add("Text Sent");
		DisplayR->GoToTextEnd();
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
	DisplayR->ReadOnly = false;
	DisplayR->SelectAll();
	DisplayR->DeleteSelection();
	DisplayR->ReadOnly = true;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::ButtonConnectToRFCOMMClick(TObject *Sender)
{

	if (ManagerConnected()) {
		try {
			SendData();
		} catch (Exception &e) {

			DisplayR->Lines->Add(e.Message);
			DisplayR->GoToTextEnd();
			FSocket->Free();
			FSocket = NULL;
		}
	}

}
//---------------------------------------------------------------------------

void __fastcall TForm1::AddDiscoveredDevice(void)
{
	ButtonDiscover->Text = "Discover devices";
	ButtonDiscover->Enabled = True;
	AniIndicator1->Visible = False;
	for(int i = 0; i < FDiscoverDevices->Count; i++) {
		ComboBoxDevices->Items->Add(FDiscoverDevices->Items[i]->DeviceName);
	}
	if (ComboBoxDevices->Items->Count > 0)
		ComboBoxDevices->ItemIndex = 0;
}

void __fastcall TForm1::DevicesDiscoveryEnd(System::TObject* const Sender,
	TBluetoothDeviceList* const ADeviceList)
{
	FDiscoverDevices = ADeviceList;
	TThread::Synchronize(NULL, AddDiscoveredDevice);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonDiscoverClick(TObject *Sender)
{
	AniIndicator1->Visible = True;
	ButtonDiscover->Text = "Discovering...";
	ButtonDiscover->Enabled = False;
	ComboBoxDevices->Clear();
	if(ManagerConnected()) {
		FAdapter = FBluetoothManager->CurrentAdapter;
		FBluetoothManager->StartDiscovery(5000);
		FBluetoothManager->OnDiscoveryEnd = DevicesDiscoveryEnd;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonOpenReadingSocketClick(TObject *Sender)
{

	if (ButtonOpenReadingSocket->IsPressed) {
		if((ServerConnectionTH == NULL) && (ManagerConnected())) {
			try {
				FAdapter = FBluetoothManager->CurrentAdapter;
				ServerConnectionTH = new TServerConnectionTH(true);
				ServerConnectionTH->FServerSocket = FAdapter->CreateServerSocket(ServiceName, StringToGUID(ServiceGUI), false);
				ServerConnectionTH->Start();
				DisplayR->Lines->Add(" - Service created: '"+ServiceName+"'");
				DisplayR->GoToTextEnd();
				ButtonOpenReadingSocket->Text = "Stop Text Service";
			}
			catch(Exception &Ex) {
				DisplayR->Lines->Add(Ex.Message);
				DisplayR->GoToTextEnd();
				ButtonOpenReadingSocket->IsPressed = False;
			}
		}
	}
	else
		if(ServerConnectionTH != NULL) {
		ServerConnectionTH->Terminate();
		ServerConnectionTH->WaitFor();
		delete ServerConnectionTH;
		ServerConnectionTH = NULL;
		DisplayR->Lines->Add(" - Service removed -");
		DisplayR->GoToTextEnd();
		ButtonOpenReadingSocket->Text = "Start Text Service";
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonPairClick(TObject *Sender)
{
	if(ComboBoxDevices->ItemIndex > -1)
		FAdapter->Pair(FDiscoverDevices->Items[ComboBoxDevices->ItemIndex]);
	else
		ShowMessage("No device selected");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonPairedDevicesClick(TObject *Sender)
{
	PairedDevices();
	ComboBoxPaired->DropDown();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonUnPairClick(TObject *Sender)
{
	if (ComboBoxPaired->ItemIndex > -1)
		FAdapter->UnPair(FPairedDevices->Items[ComboBoxPaired->ItemIndex]);
	else
		ShowMessage("No Paired device selected");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FreeSocketClick(TObject *Sender)
{
	delete FSocket;
	FSocket = NULL;
	DisplayR->Lines->Add("Client socket set free");
	DisplayR->GoToLineEnd();
}
//---------------------------------------------------------------------------
bool __fastcall TForm1::ManagerConnected(void)
{
	if(FBluetoothManager->ConnectionState == TBluetoothConnectionState::Connected) {
		Labeldiscoverable->Text = "Device discoverable as '"+
			FBluetoothManager->CurrentAdapter->AdapterName+"'";
		return true;
	}
	else {
		DisplayR->Lines->Add("No bluetooth device Found");
		DisplayR->GoToTextEnd();
		return false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
	try {
		LabelServer->Text = ServiceName;
		LabelClient->Text = "Client of " + ServiceName;
		FBluetoothManager = TBluetoothManager::Current;
		FAdapter = FBluetoothManager->CurrentAdapter;
		if(ManagerConnected()) {
			PairedDevices();
			ComboBoxPaired->ItemIndex = 0;
		}
	} catch (Exception &ex) {
		ShowMessage(ex.Message);
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonServicesClick(TObject *Sender)
{

	ListView1->Items->Clear();
	if(ComboBoxPaired->ItemIndex > -1 ){
		TBluetoothDevice * LDevice = FPairedDevices->Items[ComboBoxPaired->ItemIndex];
		AniIndicator2->Visible = True;
		TBluetoothServiceList * LServices = LDevice->GetServices();
		AniIndicator2->Visible = False;
		for(int i = 0; i < LServices->Count; i++) {
			TListViewItem * LListItem = ListView1->Items->Add();
			LListItem->ImageIndex = GetServiceImageIndex(LServices->Items[i].UUID);
			LListItem->Text =  LServices->Items[i].Name;
			if (LListItem->Text == "")
			  LListItem->Text = "<Unknown>";
			LListItem->Detail =  GUIDToString(LServices->Items[i].UUID);
		}
	}
	else {
		ShowMessage("No paired device selected");
    }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
	if(ServerConnectionTH != NULL) {
		ServerConnectionTH->Terminate();
		ServerConnectionTH->WaitFor();
		delete ServerConnectionTH;
		ServerConnectionTH = NULL;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ComboBoxPairedChange(TObject *Sender)
{
	LabelNameServer->Text =  ComboBoxPaired->Items->operator [](ComboBoxPaired->ItemIndex);
}
//---------------------------------------------------------------------------

Integer __fastcall TForm1::GetServiceImageIndex(TGUID AServiceUUID)
{
	int i;
	for(int i = 0; i < ServiceImages.size() ; i++) {
		if (ServiceImages[i].UUID == AServiceUUID) {
			return ServiceImages[i].ImageIndex;
		}
	}
	return 3;
}

