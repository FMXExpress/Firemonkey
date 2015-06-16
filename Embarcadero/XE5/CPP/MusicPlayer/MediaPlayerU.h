//---------------------------------------------------------------------------

#ifndef MediaPlayerUH
#define MediaPlayerUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <iOSApi.Foundation.hpp>
#include <FMX.MobilePreview.hpp>
#include "MusicPlayeriOS.h"
#include "MusicPlayerUtils.h"
//---------------------------------------------------------------------------
class TTFMXMusicPlayerFrm : public TForm
{
__published:	// IDE-managed Components
	TTabControl *tcUITabs;
	TTabItem *tiAlbums;
	TListView *lvAlbums;
	TTabItem *tiSongs;
	TListView *lvSongs;
	TTabItem *tiNowPlaying;
	TToolBar *tbNowPlaying;
	TButton *btnPrev;
	TButton *btnNext;
	TLayout *lyState;
	TButton *btnPlay;
	TButton *btnPause;
	TButton *btnStop;
	TLabel *lblArtist;
	TLabel *lblTitle;
	TLabel *lblAlbum;
	TLabel *lblDuration;
	TLabel *lblArtistVal;
	TLabel *lblTitleVal;
	TLabel *lblAlbumVal;
	TLabel *lblDurationVal;
	TTrackBar *tbProgress;
	TTabItem *tiSettings;
	TListBox *SettingsList;
	TListBoxGroupHeader *RepeatModes;
	TListBoxItem *All;
	TListBoxItem *One;
	TListBoxItem *None;
	TListBoxItem *Default;
	TListBoxGroupHeader *ShuffleMusic;
	TListBoxItem *ShufffleMode;
	TSwitch *swShuffleMode;
	TListBoxGroupHeader *VolumeHeader;
	TListBoxItem *VolumeListItem;
	TTrackBar *VolumeTrackBar;
	void __fastcall btnPrevClick(TObject *Sender);
	void __fastcall btnNextClick(TObject *Sender);
	void __fastcall lvAlbumsChange(TObject *Sender);
	void __fastcall btnPauseClick(TObject *Sender);
	void __fastcall btnPlayClick(TObject *Sender);
	void __fastcall btnStopClick(TObject *Sender);
	void __fastcall tbProgressChange(TObject *Sender);
	void __fastcall RepeatItemsClick(TObject *Sender);
	void __fastcall swShuffleModeSwitch(TObject *Sender);
	void __fastcall lvSongsChange(TObject *Sender);
	void __fastcall VolumeTrackBarChange(TObject *Sender);
private:	// User declarations
	void __fastcall DoUpdateUI(float newPos);
	void __fastcall UpdateNowPlaying(int newIndex);
	void __fastcall UpdateSongs();
	void __fastcall SongChanged(int newIndex);
	void __fastcall StateChanged(Musicplayer::Ios::TMPPlaybackState state);
public:		// User declarations
	__fastcall TTFMXMusicPlayerFrm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTFMXMusicPlayerFrm *TFMXMusicPlayerFrm;
//---------------------------------------------------------------------------
#endif
