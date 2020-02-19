//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MediaPlayerCommandFormH
#define MediaPlayerCommandFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <IPPeerServer.hpp>
#include <System.Tether.AppProfile.hpp>
#include <System.Tether.Manager.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ComboEdit.hpp>
#include <FMX.Edit.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TListBox *lbPlayers;
	TLabel *lbDiscoveredPlayers;
	TButton *ButtonFindPlayers;
	TButton *ButtonPlayPause;
	TTetheringManager *CommandManager;
	TTetheringAppProfile *CommandApp;
	TTrackBar *VolumeTrack;
	TComboBox *CBAdapter;
	TLabel *LabelFind;
	TComboEdit *CbEditTarget;
	TLabel *LabelVolume;
	TLabel *LabelAdapter;
	void __fastcall ButtonFindPlayersClick(TObject *Sender);
	void __fastcall ButtonPlayPauseClick(TObject *Sender);
	void __fastcall lbPlayersClick(TObject *Sender);
	void __fastcall CommandManagerRequestManagerPassword(const TObject *Sender, const UnicodeString RemoteIdentifier, UnicodeString &Password);
	void __fastcall CommandManagerEndManagersDiscovery(const TObject *Sender, const TTetheringManagerInfoList *RemoteManagers);
	void __fastcall CommandManagerEndProfilesDiscovery(const TObject *Sender, const TTetheringProfileInfoList *RemoteProfiles);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall VolumeTrackChange(TObject *Sender);
	void __fastcall CBAdapterChange(TObject *Sender);
	void __fastcall CommandManagerNewManager(TObject * const Sender, const TTetheringManagerInfo &AManagerInfo);
	void __fastcall CommandManagerRemoteManagerShutdown(TObject * const Sender, const UnicodeString AManagerIdentifier);

private:	// User declarations
	TFormatSettings FInvariantFormatSettings;

	bool __fastcall CheckMediaPlayers(void);
	void __fastcall RefreshList(void);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
