
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
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
  lbPlayers->Clear();
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
  int i;
  for (i = 0; i < RemoteManagers->Count; i++) {
	CommandManager->PairManager(const_cast<TTetheringManagerInfoList *>(RemoteManagers)->Items[i]);
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CommandManagerEndProfilesDiscovery(const TObject *Sender, const TTetheringProfileInfoList *RemoteProfiles)

{
  int i;
  lbPlayers->Clear();
  for (i = 0; i < CommandManager->RemoteProfiles->Count; i++) {
	if (CommandManager->RemoteProfiles->Items[i].ProfileText == "FMXMediaPlayer")
	{
	  lbPlayers->Items->Add(CommandManager->RemoteProfiles->Items[i].ProfileText);
	}
  }

  if (lbPlayers->Count > 0)
  {
	lbPlayers->ItemIndex = 0;
	CommandApp->Connect(CommandManager->RemoteProfiles->Items[0]);  // Connect to the first one
  }
}
//---------------------------------------------------------------------------

