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

#include "MediaPlayerCommandForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonFindPlayersClick(TObject *Sender)
{
	((TButton *)Sender)->Enabled = false;
	if (CommandManager->Enabled == false)
		CommandManager->Enabled = true;

	for (int i = CommandManager->PairedManagers->Count - 1; i >= 0; i--)
		CommandManager->UnPairManager(CommandManager->PairedManagers->Items[i]);

	lbPlayers->Items->Clear();
	lbPlayers->Items->Add("Finding Players ...");

	if (CbEditTarget->ItemIndex != -1)
		CommandManager->DiscoverManagers((*CbEditTarget->Items)[CbEditTarget->ItemIndex]);
	else if (CbEditTarget->Text != "")
		CommandManager->DiscoverManagers(CbEditTarget->Text);
	else
		CommandManager->DiscoverManagers();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ButtonPlayPauseClick(TObject *Sender)
{
  CommandApp->RunRemoteAction(CommandManager->RemoteProfiles->Items[lbPlayers->ItemIndex], "acPlayPause");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::lbPlayersClick(TObject *Sender)
{
  CommandApp->Connect(CommandManager->RemoteProfiles->Items[lbPlayers->ItemIndex]);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CommandManagerRequestManagerPassword(const TObject *Sender, const UnicodeString RemoteIdentifier,
		  UnicodeString &Password)
{
  Password = "1234";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CommandManagerEndManagersDiscovery(const TObject *Sender, const TTetheringManagerInfoList *RemoteManagers)
{
	if (RemoteManagers->Count == 0) {
		RefreshList();
	}
	else {
		lbPlayers->Items->Clear();
		lbPlayers->Items->Add("Pairing Players");
		for (int i = 0; i < RemoteManagers->Count; i++) {
			TTetheringManagerInfo LManagerInfo = const_cast<TTetheringManagerInfoList *>(RemoteManagers)->Items[i];
			if (LManagerInfo.ManagerText == "FMXManager")
				CommandManager->PairManager(LManagerInfo);
		}
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CommandManagerEndProfilesDiscovery(const TObject *Sender, const TTetheringProfileInfoList *RemoteProfiles)
{
	RefreshList();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  FInvariantFormatSettings = TFormatSettings::Create();
  FInvariantFormatSettings.DecimalSeparator = '.';
  FInvariantFormatSettings.ThousandSeparator = ',';
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject *Sender)
{
  CommandManager->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::VolumeTrackChange(TObject *Sender)
{
  if (CheckMediaPlayers())
  {
	TVarRec LVarRec[1];
	LVarRec[0] = (long double)VolumeTrack->Value;
	String aux = Format("%f", LVarRec, 0, FInvariantFormatSettings);
	CommandApp->SendString(CommandManager->RemoteProfiles->Items[lbPlayers->ItemIndex], "VolumeTrack", aux);
  }
}
//---------------------------------------------------------------------------

bool __fastcall TForm1::CheckMediaPlayers(void)
{
  if ((CommandManager->RemoteProfiles->Count > 0) && (lbPlayers->ItemIndex >= 0))
	return true;
  else
  {
	if (CommandManager->RemoteProfiles->Count > 0)
		ShowMessage("Select a MediaPlayer from the list to connect, please");
	return false;
  }
}
//---------------------------------------------------------------------------
String GetConnections(const TTetheringAllowedConnections &AConnections)
{
	TTetheringAllowedConnection LConnection;
	String LResult;

	TVarRec LVarRec[3];
	LResult = "[";
	for (int i = 0; i < AConnections.Length; i++)
	{
	  LVarRec[0] = AConnections[i].ProtocolType;
	  LVarRec[1] = AConnections[i].AdapterType;
	  LVarRec[2] = AConnections[i].Connection;
	  LResult = LResult + Format("'%s:%s;%s',", LVarRec, 3);
	}

	LResult[LResult.Length() - 1] = ']';

	return LResult;
}

void __fastcall TForm1::RefreshList(void)
{
  lbPlayers->Clear();
  for (int i = 0; i < CommandManager->RemoteProfiles->Count; i++) {
	TTetheringProfileInfo LProfileInfo;
	LProfileInfo = CommandManager->RemoteProfiles->Items[i];
	if (LProfileInfo.ProfileText == "FMXMediaPlayer")
	  lbPlayers->Items->Add(LProfileInfo.ProfileText + GetConnections(LProfileInfo.AllowedConnections));
  }

  if (lbPlayers->Count > 0)
  {
	// Connect to the first one or the last valid index used
	if ((lbPlayers->ItemIndex == -1) || (lbPlayers->ItemIndex >= lbPlayers->Count))
	  lbPlayers->ItemIndex = 0;
	CommandApp->Connect(CommandManager->RemoteProfiles->Items[lbPlayers->ItemIndex]);
  }
  else
  {
	  lbPlayers->ItemIndex = -1;
	  lbPlayers->Items->Add("No Players Found!!");
  }
  ButtonFindPlayers->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAdapterChange(TObject *Sender)
{
  CommandManager->Enabled = false;
  CommandManager->AllowedAdapters = (*(CBAdapter->Items))[CBAdapter->ItemIndex];
  CommandManager->Enabled = true;
  CbEditTarget->Items->Clear();
  CbEditTarget->Items->Add("");
  CbEditTarget->ItemIndex = -1; // Select none
  if (CommandManager->AllowedAdapters == "Network")
  {
	CbEditTarget->Items->Add("192.168.1.0");
	CbEditTarget->Items->Add("TargetHost1;TargetHost2");
	CbEditTarget->Items->Add("192.168.1.123");
  }
  else
	CbEditTarget->Items->Add("5c:f3:70:61:15:c4");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CommandManagerNewManager(TObject * const Sender, const TTetheringManagerInfo &AManagerInfo)
{
  if (AManagerInfo.ManagerText == "FMXManager")
	CommandManager->PairManager(AManagerInfo);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CommandManagerRemoteManagerShutdown(TObject * const Sender, const UnicodeString AManagerIdentifier)
{
	RefreshList();
}
//---------------------------------------------------------------------------

