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
	void __fastcall ButtonFindPlayersClick(TObject *Sender);
	void __fastcall ButtonPlayPauseClick(TObject *Sender);
	void __fastcall lbPlayersClick(TObject *Sender);
	void __fastcall CommandManagerRequestManagerPassword(const TObject *Sender, const UnicodeString RemoteIdentifier, UnicodeString &Password);
	void __fastcall CommandManagerEndManagersDiscovery(const TObject *Sender, const TTetheringManagerInfoList *RemoteManagers);
	void __fastcall CommandManagerEndProfilesDiscovery(const TObject *Sender, const TTetheringProfileInfoList *RemoteProfiles);






private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
