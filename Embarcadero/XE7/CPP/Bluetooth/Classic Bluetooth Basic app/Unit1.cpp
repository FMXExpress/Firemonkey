//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;

String Msg = "";
const String ServiceName = "Basic Text Server";
const String ServiceGUI = "{B62C4E8D-62CC-404B-BBBF-BF3E3BBB1378}";
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
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

void __fastcall TForm1::ButtonCloseReadingSocketClick(TObject *Sender)
{
	if(ServerConnectionTH != NULL) {
		ServerConnectionTH->Terminate();
		ServerConnectionTH->WaitFor();
		delete ServerConnectionTH;
		ServerConnectionTH = NULL;
		DisplayR->Lines->Add(" - Service removed -");
		DisplayR->GoToTextEnd();
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonConnectToRFCOMMClick(TObject *Sender)
{
	SendData();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DevicesDiscoveryEnd(System::TObject* const Sender,
	TBluetoothDeviceList* const ADeviceList)
{
	FDiscoverDevices = ADeviceList;
	for(int i = 0; i < ADeviceList->Count; i++) {
		ComboBoxDevices->Items->Add(ADeviceList->Items[i]->DeviceName + " -> " +
			ADeviceList->Items[i]->Address);
	}
	ComboBoxDevices->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonDiscoverClick(TObject *Sender)
{
	ComboBoxDevices->Clear();
	if(ManagerConnected()) {
		FAdapter = FBluetoothManager->CurrentAdapter;
		FBluetoothManager->StartDiscovery(10000);
		FBluetoothManager->OnDiscoveryEnd = DevicesDiscoveryEnd;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonOpenReadingSocketClick(TObject *Sender)
{
	if((ServerConnectionTH == NULL) && (ManagerConnected())) {
		try {
			FAdapter = FBluetoothManager->CurrentAdapter;
			ServerConnectionTH = new TServerConnectionTH(true);
			ServerConnectionTH->FServerSocket = FAdapter->CreateServerSocket(ServiceName, StringToGUID(ServiceGUI), false);
			ServerConnectionTH->Start();
			DisplayR->Lines->Add(" - Service created: '"+ServiceName+"'");
			DisplayR->GoToTextEnd();
		}
		catch(Exception &Ex) {
			DisplayR->Lines->Add(Ex.Message);
			DisplayR->GoToTextEnd();
		}
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
	ComboBoxServices->Clear();
	if(ComboBoxPaired->ItemIndex > -1 ){
		TBluetoothDevice * LDevice = FPairedDevices->Items[ComboBoxPaired->ItemIndex];
		TBluetoothServiceList * LServices = LDevice->GetServices();
		for(int i = 0; i < LServices->Count; i++) {
			ComboBoxServices->Items->Add(LServices->Items[i].Name + " --> " +
				GUIDToString(LServices->Items[i].UUID));
		}
		ComboBoxServices->ItemIndex = 0;
		ComboBoxServices->DropDown();
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

