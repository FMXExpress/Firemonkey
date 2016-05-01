// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MediaPlayerU.h"

// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TTFMXMusicPlayerFrm *TFMXMusicPlayerFrm;
#ifdef __APPLE__
using namespace Musicplayer::Ios;
#endif
#ifdef __ANDROID__
using namespace Musicplayer::Android;
#endif

// ---------------------------------------------------------------------------
__fastcall TTFMXMusicPlayerFrm::TTFMXMusicPlayerFrm(TComponent* Owner)
	: TForm(Owner) { }

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPrevClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Previous();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::btnNextClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Next();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::DoUpdateUI(float newPos) {
	TNotifyEvent handler = tbProgress->OnChange;
	tbProgress->OnChange = NULL;
	tbProgress->Value = newPos;
	tbProgress->OnChange = handler;
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateNowPlaying(int newIndex) {
	lblArtistVal->Text = TMusicPlayer::DefaultPlayer()
		->Playlist[newIndex].Artist;
	lblTitleVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Title;
	lblAlbumVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Album;
	lblDurationVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex]
		.Duration;
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateSongs() {
	lvSongs->BeginUpdate();
	lvSongs->ClearItems();
	for (int i = 0; i < TMusicPlayer::DefaultPlayer()->Playlist.Length; i++) {
		Musicplayer::Utils::TMPSong song =
			TMusicPlayer::DefaultPlayer()->Playlist[i];
		TListViewItem * item = lvSongs->Items->Add();
		if (song.Artist != "Unknow") {
			item->Text =
				Format("%s - %s", ARRAYOFCONST((song.Artist, song.Title)));
		} else {
			item->Text = song.Title;
		}
	}
	lvSongs->EndUpdate();
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::SongChanged(int newIndex) {
	TNotifyEvent handler = lvSongs->OnChange;
	lvSongs->OnChange = NULL;
	lvSongs->ItemIndex = newIndex;
	UpdateNowPlaying(newIndex);
	lvSongs->OnChange = handler;
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::StateChanged
	(Musicplayer::Utils::TMPPlaybackState state) {
	btnPlay->Enabled =
		!(state == Musicplayer::Utils::TMPPlaybackState::Playing);
	btnPause->Enabled =
		!(state == Musicplayer::Utils::TMPPlaybackState::Paused ||
		state == Musicplayer::Utils::TMPPlaybackState::Stopped);
	btnStop->Enabled =
		!(state == Musicplayer::Utils::TMPPlaybackState::Stopped);
	tbProgress->Enabled =
		!(state == Musicplayer::Utils::TMPPlaybackState::Paused ||
		state == Musicplayer::Utils::TMPPlaybackState::Stopped);
	btnNext->Enabled =
		(!(state == Musicplayer::Utils::TMPPlaybackState::Stopped)) && TMusicPlayer::DefaultPlayer()->CanSkipForward();
	btnPrev->Enabled =
		(!(state == Musicplayer::Utils::TMPPlaybackState::Stopped)) && TMusicPlayer::DefaultPlayer()->CanSkipBack();
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::lvAlbumsChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->GetSongsInAlbum
		(TMusicPlayer::DefaultPlayer()->Albums[lvAlbums->ItemIndex].Name);
	UpdateSongs();
	tcUITabs->SetActiveTabWithTransition(tiSongs, TTabTransition::Slide);
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPauseClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Pause();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Paused);
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPlayClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Play();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnStopClick(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Stop();
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Stopped);
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::tbProgressChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Time =
		(tbProgress->Value * TMusicPlayer::DefaultPlayer()->Duration) / 100;
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::RepeatItemsClick(TObject *Sender) {
	if (dynamic_cast<TListBoxItem*>(Sender)) {
		for (int i = 0; i < SettingsList->Items->Count; i++) {
			SettingsList->ItemByIndex(i)->ItemData->Accessory =
				TListBoxItemData::TAccessory::aNone; ;
			TListBoxItem * item = (TListBoxItem*)Sender;
			if (item->Text == "All")
				TMusicPlayer::DefaultPlayer()->RepeatMode =
					Musicplayer::Utils::TMPRepeatMode::All;
			if (item->Text == "One")
				TMusicPlayer::DefaultPlayer()->RepeatMode =
					Musicplayer::Utils::TMPRepeatMode::One;
			if (item->Text == "None")
				TMusicPlayer::DefaultPlayer()->RepeatMode =
					Musicplayer::Utils::TMPRepeatMode::None;
			if (item->Text == "Default")
				TMusicPlayer::DefaultPlayer()->RepeatMode =
					Musicplayer::Utils::TMPRepeatMode::Default;
			item->ItemData->Accessory =
				TListBoxItemData::TAccessory::aCheckmark;
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::swShuffleModeSwitch(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->ShuffleMode = swShuffleMode->IsChecked;
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::lvSongsChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->PlayByIndex(lvSongs->ItemIndex);
	UpdateNowPlaying(lvSongs->ItemIndex);
	tcUITabs->SetActiveTabWithTransition(tiNowPlaying, TTabTransition::Slide);
	StateChanged(Musicplayer::Utils::TMPPlaybackState::Playing);
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::VolumeTrackBarChange(TObject *Sender) {
	TMusicPlayer::DefaultPlayer()->Volume = VolumeTrackBar->Value;
}
// ---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::volTimerTimer(TObject *Sender)
{
//	FSynchronizing = true;
	TNotifyEvent LEvent = VolumeTrackBar->OnChange;
	VolumeTrackBar->OnChange = NULL;
	VolumeTrackBar->Value = TMusicPlayer::DefaultPlayer()->Volume;
	VolumeTrackBar->OnChange = LEvent;
//	FSynchronizing = false;
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::FormCreate(TObject *Sender)
{
#ifdef __ANDROID__
	tcUITabs->TabPosition = TTabPosition::Top;
#endif
	TMusicPlayer::DefaultPlayer()->OnSongChange = SongChanged;
	TMusicPlayer::DefaultPlayer()->OnProcessPlay = DoUpdateUI;
	TMusicPlayer::DefaultPlayer()->GetAlbums();
	TMusicPlayer::DefaultPlayer()->GetSongs();
	if (TMusicPlayer::DefaultPlayer()->Albums.Length >= 2) {
		lvAlbums->BeginUpdate();
		for (int i = 0; i < TMusicPlayer::DefaultPlayer()->Albums.Length; i++) {
			auto album = TMusicPlayer::DefaultPlayer()->Albums[i];
			auto item = lvAlbums->Items->Add();
			item->Text = album.Name;
			item->Detail = album.Artist;
			item->Bitmap = album.Artwork;
		}
		lvAlbums->EndUpdate();
		UpdateSongs();
		TMusicPlayer::DefaultPlayer()->ShuffleMode = swShuffleMode->IsChecked;
		RepeatItemsClick(All);
		StateChanged(Musicplayer::Utils::TMPPlaybackState::Stopped);
	} else {
		ShowMessage("There is no music on this device");
	}
}
//---------------------------------------------------------------------------



void __fastcall TTFMXMusicPlayerFrm::btnCloseSettingsClick(TObject *Sender)
{
	mvSettings->HideMaster();
}
//---------------------------------------------------------------------------

