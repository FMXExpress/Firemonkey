// ---------------------------------------------------------------------------

#pragma hdrstop

#include <System.Classes.hpp>
#include "MusicPlayeriOS.h"
#include "MusicPlayerUtils.h"

#ifdef __APPLE__
using namespace Musicplayer::Utils;

namespace Musicplayer {
namespace Ios {

TMusicPlayer* TMusicPlayer::FInstance;

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::Pause(void) {
	FMusicPlayer->pause();
}

// ---------------------------------------------------------------------------
__fastcall TMusicPlayer::TMusicPlayer(TMPControllerType AType) {
	switch (AType) {
	case TMPControllerType::App:
		FMusicPlayer = TMPMusicPlayerController::Wrap
			(TMPMusicPlayerController::OCClass->
			applicationMusicPlayer());
		break;
	case TMPControllerType::Ipod:
		FMusicPlayer = TMPMusicPlayerController::Wrap
			(TMPMusicPlayerController::OCClass->iPodMusicPlayer());
		break;
	}
	FDefaultAlbumImage =
		new TBitmap(TPath::GetDocumentsPath() + "/MusicNote.png");
	(new TProcessThread(true, this, DoOnProcessPlay))->Start();
}

// ---------------------------------------------------------------------------
TMusicPlayer* __fastcall TMusicPlayer::DefaultPlayer() {
	if (FInstance == nullptr) {
		FInstance = new TMusicPlayer(TMPControllerType::App);
	}
	return FInstance;
}

// ---------------------------------------------------------------------------
__fastcall TMusicPlayer::~TMusicPlayer(void) {
	FMusicPlayer->release();
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::DoOnSongChange(int newIndex) {
	if (FOnSongChange) {
		TThread::Queue(TThread::CurrentThread,
			new MyThreadMethod<TOnSongChangeEvent, int>(FOnSongChange,
			newIndex));
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::DoOnProcessPlay(float newPos) {
	if (FOnProcessPlay) {
		TThread::Queue(TThread::CurrentThread,
			new MyThreadMethod<TOnProcessPlayEvent, float>
			(FOnProcessPlay, newPos));
	}
}

// ---------------------------------------------------------------------------
System::DynamicArray<System::UnicodeString>__fastcall
	TMusicPlayer::GetAlbums(void) {
	System::DynamicArray<System::UnicodeString>_return;
	_di_MPMediaQuery query =
		TMPMediaQuery::Wrap(TMPMediaQuery::OCClass->albumsQuery());
	_return.set_length(query->collections()->count().get());
	FAlbums.set_length(query->collections()->count().get() + 1);
	FAlbums[query->collections()->count().get()] =
		TMPAlbum::AllMusicAlbum();
	for (int i = 0; i < query->collections()->count().get(); i++) {
		_di_MPMediaItemCollection item =
			TMPMediaItemCollection::Wrap
			(query->collections()->objectAtIndex(NativeUInt(i)));
		FAlbums[i].Name =
			TNSString::Wrap(item->representativeItem()->valueForProperty
			(MPMediaItemPropertyAlbumTitle()))->UTF8String();
		FAlbums[i].Artist =
			TNSString::Wrap(item->representativeItem()->valueForProperty
			(MPMediaItemPropertyArtist()))->UTF8String();
		FAlbums[i].Album_ID = i;
		if (item->valueForProperty(MPMediaItemPropertyArtwork())
			!= nullptr) {
			auto artwork =
				TMPMediaItemArtwork::Wrap
				(item->valueForProperty(MPMediaItemPropertyArtwork()));
			NSSize art_size;
			art_size.width = 200;
			art_size.height = 200;
			UIImage *art_img = artwork->imageWithSize(art_size);
			FAlbums[i].Artwork =
				UIImageToBitmap(art_img, 0, TSize(1024, 1024));
		}
		else {
			FAlbums[i].Artwork = FDefaultAlbumImage;
		}
		_return[i] = FAlbums[i].Name;
	}
	return _return;
}

// ---------------------------------------------------------------------------
float __fastcall TMusicPlayer::GetDuration(void) {
	return TNSNumber::Wrap
		(FMusicPlayer->nowPlayingItem()->valueForProperty
		(MPMediaItemPropertyPlaybackDuration()))->floatValue();
}

// ---------------------------------------------------------------------------
TMPPlaybackState __fastcall TMusicPlayer::GetPlaybackState(void) {
	return TMPPlaybackState(FMusicPlayer->playbackState().get());
}

// ---------------------------------------------------------------------------
TMPRepeatMode __fastcall TMusicPlayer::GetRepeatMode(void) {
	return TMPRepeatMode(FMusicPlayer->repeatMode().get());
}

// ---------------------------------------------------------------------------
bool __fastcall TMusicPlayer::GetShuffleMode(void) {
	return (FMusicPlayer->shuffleMode() == MPMusicShuffleModeSongs) ||
		(FMusicPlayer->shuffleMode() == MPMusicShuffleModeAlbums);
}

// ---------------------------------------------------------------------------
System::DynamicArray<System::UnicodeString>__fastcall
	TMusicPlayer::GetSongs(void) {
	System::DynamicArray<System::UnicodeString>_return;
	_di_MPMediaQuery query =
		TMPMediaQuery::Wrap(TMPMediaQuery::OCClass->songsQuery());
	FMusicPlayer->setQueueWithQuery(query);
	_return.set_length(query->items()->count().get());
	FPlaylist.set_length(query->items()->count().get());
	for (int i = 0; i < query->items()->count().get(); i++) {
		FPlaylist[i] =
			TMPSong::FromMediaItem
			(TMPMediaItem::Wrap(query->items()->objectAtIndex
			(NativeUInt(i))));
		_return[i] =
			Format("[%s]-[%s]",
			ARRAYOFCONST((FPlaylist[i].Artist, FPlaylist[i].Title)));
	}
	return _return;
}

// ---------------------------------------------------------------------------
System::DynamicArray<System::UnicodeString>__fastcall
	TMusicPlayer::GetSongsInAlbum(System::UnicodeString AName) {
	if (AName == TMPAlbum::AllMusicAlbum().Name) {
		return this->GetSongs();
	}
	System::DynamicArray<System::UnicodeString>_return;
	_di_MPMediaQuery query =
		TMPMediaQuery::Wrap(TMPMediaQuery::Alloc()->init());
	_di_MPMediaPropertyPredicate predicate =
		TMPMediaPropertyPredicate::Wrap
		(TMPMediaPropertyPredicate::OCClass->predicateWithValue
		(TNSString::OCClass->stringWithString(NSSTR(AName)),
		MPMediaItemPropertyAlbumTitle()));
	query->addFilterPredicate(predicate);
	query->setGroupingType(NativeInt(MPMediaGroupingAlbum));
	FMusicPlayer->setQueueWithQuery(query);
	_return.set_length(query->items()->count().get());
	FPlaylist.set_length(query->items()->count().get());
	for (int i = 0; i < query->items()->count().get(); i++) {
		FPlaylist[i] =
			TMPSong::FromMediaItem
			(TMPMediaItem::Wrap(query->items()->objectAtIndex
			(NativeUInt(i))));
		_return[i] =
			Format("[%s]-[%s]",
			ARRAYOFCONST((FPlaylist[i].Artist, FPlaylist[i].Title)));
	}
	return _return;
}

// ---------------------------------------------------------------------------
float __fastcall TMusicPlayer::GetTime(void) {
	return FMusicPlayer->currentPlaybackTime();
}

// ---------------------------------------------------------------------------
float __fastcall TMusicPlayer::GetVolume(void) {
	return FMusicPlayer->volume();
}

// ---------------------------------------------------------------------------
NativeUInt __fastcall TMusicPlayer::IndexOfNowPlayingItem(void) {
	int _return = 0;
	for (int i = 0; i < FPlaylist.Length; i++) {
		if (FPlaylist[i].Equals(TMPSong::FromMediaItem
			(FMusicPlayer->nowPlayingItem()))) {
			_return = i;
			FCurrentIndex = i;
			break;
		}
	}
	return NativeUInt(_return);
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::Next(void) {
	FMusicPlayer->skipToNextItem();
	DoOnSongChange(IndexOfNowPlayingItem());
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::Play(void) {
	if (FMusicPlayer->playbackState().get() == (int)
		Utils::TMPPlaybackState::Stopped) {
		FMusicPlayer->setNowPlayingItem
			(FPlaylist[FCurrentIndex].MPItem);
	}
	FMusicPlayer->play();
}

//---------------------------------------------------------------------------
bool __fastcall TMusicPlayer::CanSkipBack() {
	Utils::TMPRepeatMode LRepeatMode = Utils::TMPRepeatMode(FMusicPlayer->repeatMode().get());
	switch (LRepeatMode) {
		case Utils::TMPRepeatMode::Default:
		case Utils::TMPRepeatMode::None: return FCurrentIndex > FPlaylist.Low;
	default:
		return true;
	}
}

//---------------------------------------------------------------------------
bool __fastcall TMusicPlayer::CanSkipForward() {
	Utils::TMPRepeatMode LRepeatMode = Utils::TMPRepeatMode(FMusicPlayer->repeatMode().get());
	switch (LRepeatMode) {
		case Utils::TMPRepeatMode::Default:
		case Utils::TMPRepeatMode::None: return FCurrentIndex < FPlaylist.High;
	default:
		return true;
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::PlayByIndex(unsigned Index) {
	if ((Index >= 0) && (Index < FPlaylist.Length)) {
		FMusicPlayer->setNowPlayingItem(FPlaylist[Index].MPItem);
		FCurrentIndex = Index;
	}
	Play();
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::Previous(void) {
	FMusicPlayer->skipToPreviousItem();
	DoOnSongChange(IndexOfNowPlayingItem());
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::SetPlayerType(TMPControllerType AType) {
	if (FInstance) {
		FInstance->DisposeOf();
	}
	FInstance = new TMusicPlayer(AType);
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::SetRepeatMode(const TMPRepeatMode Value) {
	FMusicPlayer->setRepeatMode
		(static_cast<NativeInt>(static_cast<int>(Value)));
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::SetShuffleMode(const bool Value) {
	if (Value) {
		FMusicPlayer->setShuffleMode
			(NativeInt(MPMusicShuffleModeSongs));
	}
	else {
		FMusicPlayer->setShuffleMode(NativeInt(MPMusicShuffleModeOff));
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::SetTime(const float Value) {
	FMusicPlayer->setCurrentPlaybackTime(Value);
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::SetVolume(const float Value) {
	FMusicPlayer->setVolume(Value);
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::Stop(void) {
	FMusicPlayer->stop();
	DoOnProcessPlay(0);
}

// ------------ TMusicPlayer::TProcessThread --------------------------------
__fastcall TMusicPlayer::TProcessThread::TProcessThread
	(bool CreateSuspended, TMusicPlayer* AMusicPlayer,
	Musicplayer::Utils::TOnProcessPlayEvent processHandler)
	: TThread(CreateSuspended) {
	FMusicPlayer = AMusicPlayer;
	FLastItem = TMPSong::EmptySong();
	FOnProcessPlay = processHandler;
}

// ---------------------------------------------------------------------------
__fastcall TMusicPlayer::TProcessThread::~TProcessThread(void) {
	FMusicPlayer = nullptr;
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::TProcessThread::Execute(void) {
	while (FMusicPlayer) {
		switch (FMusicPlayer->PlaybackState) {
		case TMPPlaybackState::Playing:
			ProcessPlay();
			break;
		case TMPPlaybackState::Stopped:
		case TMPPlaybackState::Paused:
		case TMPPlaybackState::Interrupted:
		case TMPPlaybackState::SeekingForward:
		case TMPPlaybackState::SeekingBackward:
			Sleep(100);
			break;
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMusicPlayer::TProcessThread::ProcessPlay(void) {
	if (FOnProcessPlay) {
		FOnProcessPlay(
			(FMusicPlayer->Time / FMusicPlayer->Duration) * 100);
	}
	if (FLastItem.Equals(TMPSong::EmptySong())) {
		FLastItem = FMusicPlayer->Playlist[FMusicPlayer->CurrentIndex];
	}
	else {
		if (!FLastItem.Equals
			(FMusicPlayer->Playlist[FMusicPlayer->IndexOfNowPlayingItem
			().get()])) {
			FLastItem =
				FMusicPlayer->Playlist[FMusicPlayer->CurrentIndex];
			FMusicPlayer->DoOnSongChange(FMusicPlayer->CurrentIndex);
			Sleep(100);
		}
		else {
			Sleep(100);
		}
	}
}
	// ---------------------------------------------------------------------------
} /* namespace Ios */
} /* namespace Musicplayer */
#endif
#pragma package(smart_init)
