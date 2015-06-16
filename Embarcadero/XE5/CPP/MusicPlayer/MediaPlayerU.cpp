//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MediaPlayerU.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TTFMXMusicPlayerFrm *TFMXMusicPlayerFrm;

using namespace Musicplayer::Ios;

//---------------------------------------------------------------------------
__fastcall TTFMXMusicPlayerFrm::TTFMXMusicPlayerFrm(TComponent* Owner)
	: TForm(Owner)
{
	TMusicPlayer::DefaultPlayer()->OnSongChange = SongChanged;
	TMusicPlayer::DefaultPlayer()->OnProcessPlay = DoUpdateUI;
	TMusicPlayer::DefaultPlayer()->GetAlbums();
	TMusicPlayer::DefaultPlayer()->GetSongs();
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
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPrevClick(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Previous();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::btnNextClick(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Next();
	StateChanged(TMusicPlayer::DefaultPlayer()->PlaybackState);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::DoUpdateUI(float newPos)
{
	TNotifyEvent handler = tbProgress->OnChange;
	tbProgress->OnChange = NULL;
	tbProgress->Value = newPos;
	tbProgress->OnChange = handler;
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateNowPlaying(int newIndex)
{
	lblArtistVal->Text   = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Artist;
	lblTitleVal->Text    = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Title;
	lblAlbumVal->Text    = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Album;
	lblDurationVal->Text = TMusicPlayer::DefaultPlayer()->Playlist[newIndex].Duration;
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::UpdateSongs()
{
	lvSongs->BeginUpdate();
	lvSongs->ClearItems();
	for (int i = 0; i < TMusicPlayer::DefaultPlayer()->Playlist.Length; i++) {
		Musicplayer::Utils::TMPSong song = TMusicPlayer::DefaultPlayer()->Playlist[i];
		TListViewItem * item = lvSongs->Items->Add();
		if (song.Artist != "Unknow") {
			item->Text = Format("%s - %s", ARRAYOFCONST((song.Artist, song.Title)));
		} else {
            item->Text = song.Title;
        }
	}
	lvSongs->EndUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::SongChanged(int newIndex)
{
	TNotifyEvent handler = lvSongs->OnChange;
	lvSongs->OnChange = NULL;
	lvSongs->ItemIndex = newIndex;
	UpdateNowPlaying(newIndex);
	lvSongs->OnChange = handler;
	StateChanged(TMPPlaybackState::psPlaying);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::StateChanged(Musicplayer::Ios::TMPPlaybackState state)
{
	btnPlay->Enabled  = !(state == TMPPlaybackState::psPlaying);
	btnPause->Enabled = !(state == TMPPlaybackState::psPaused);
	btnStop->Enabled  = !(state == TMPPlaybackState::psStopped);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::lvAlbumsChange(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->GetSongsInAlbum(TMusicPlayer::DefaultPlayer()->
		Albums[lvAlbums->ItemIndex].Name);
	UpdateSongs();
	tcUITabs->SetActiveTabWithTransition(tiSongs, TTabTransition::ttSlide);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPauseClick(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Pause();
	StateChanged(TMPPlaybackState::psPaused);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnPlayClick(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Play();
	StateChanged(TMPPlaybackState::psPlaying);
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::btnStopClick(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Stop();
	StateChanged(TMPPlaybackState::psStopped);
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::tbProgressChange(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Time =
		(tbProgress->Value * TMusicPlayer::DefaultPlayer()->Duration)/100;
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::RepeatItemsClick(TObject *Sender)
{
	if (dynamic_cast<TListBoxItem*>(Sender)) {
		for (int i = 0;i < SettingsList->Items->Count; i++) {
			SettingsList->ItemByIndex(i)->ItemData->Accessory =
				TListBoxItemData::TAccessory::aNone;;
			TListBoxItem * item = (TListBoxItem*)Sender;
			if (item->Text == "All")
				TMusicPlayer::DefaultPlayer()->RepeatMode = TMPRepeatMode::rmAll;
			if (item->Text == "One")
				TMusicPlayer::DefaultPlayer()->RepeatMode = TMPRepeatMode::rmOne;
			if (item->Text == "None")
				TMusicPlayer::DefaultPlayer()->RepeatMode = TMPRepeatMode::rmNone;
			if (item->Text == "Default")
				TMusicPlayer::DefaultPlayer()->RepeatMode = TMPRepeatMode::rmDefault;
			item->ItemData->Accessory = TListBoxItemData::TAccessory::aCheckmark;
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TTFMXMusicPlayerFrm::swShuffleModeSwitch(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->ShuffleMode = swShuffleMode->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::lvSongsChange(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->PlayByIndex(lvSongs->ItemIndex);
	UpdateNowPlaying(lvSongs->ItemIndex);
	tcUITabs->SetActiveTabWithTransition(tiNowPlaying, TTabTransition::ttSlide);
	StateChanged(TMPPlaybackState::psPlaying);
}
//---------------------------------------------------------------------------

void __fastcall TTFMXMusicPlayerFrm::VolumeTrackBarChange(TObject *Sender)
{
	TMusicPlayer::DefaultPlayer()->Volume = VolumeTrackBar->Value;
}
//---------------------------------------------------------------------------

