// ---------------------------------------------------------------------------

#pragma hdrstop

#include "MusicPlayerUtils.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#include "MusicPlayerUtils.h"

namespace Musicplayer {
namespace Utils {
		// ---------------------------------------------------------------------------
#ifdef __APPLE__

TMPSong __fastcall TMPSong::FromMediaItem
	(Iosapi::Mediaplayer::_di_MPMediaItem Item) {
	TMPSong song;
	song.Artist =
		TNSString::Wrap
		(Item->valueForProperty(MPMediaItemPropertyArtist()))
		->UTF8String();
	song.Album =
		TNSString::Wrap
		(Item->valueForProperty(MPMediaItemPropertyAlbumTitle()))
		->UTF8String();
	song.Title =
		TNSString::Wrap
		(Item->valueForProperty(MPMediaItemPropertyTitle()))
		->UTF8String();
	song.Duration =
		TMPSong::DurationToString
		(TNSNumber::Wrap(Item->valueForProperty
		(MPMediaItemPropertyPlaybackDuration()))->floatValue());
	song.MPItem = Item;
	song.MPItem->retain();
	if (song.Artist == "") {
		song.Artist = "Unknown";
	}
	return song;
}
#endif

#ifdef __ANDROID__

TMPSong __fastcall TMPSong::FromCursor
	(Androidapi::Jni::Graphicscontentviewtext::_di_JCursor c) {
	TMPSong song;
	song.Artist = JStringToString(c->getString(0));
	if (song.Artist == "<unknown>")
		song.Artist = "Unknown";
	song.Title = JStringToString(c->getString(1));
	song.Path = JStringToString(c->getString(2));
	song.Album = JStringToString(c->getString(3));
	song.Duration = TMPSong::DurationToString(c->getFloat(4));
	return song;
}
#endif

// ---------------------------------------------------------------------------
TMPSong TMPSong::EmptySong() {
	TMPSong song;
	song.Album = "-";
	song.Artist = "-";
	song.Duration = "-";
	song.Title = "-";
	return song;
}

// ---------------------------------------------------------------------------
System::UnicodeString TMPSong::DurationToString(float duration) {
	System::UnicodeString _return = "";
	System::UnicodeString _secondsStr = "";
#ifdef __APPLE__
	int hours = trunc(duration) / (60 * 60);
	int minutes = (static_cast<int>(trunc(duration)) % (60 * 60)) / 60;
	int seconds = static_cast<int>(trunc(duration)) % 60;
#endif
#ifdef __ANDROID__
	int hours = trunc(duration) / (1000 * 60 * 60);
	int minutes = (static_cast<int>(trunc(duration)) % (1000 * 60 * 60))
		/ (1000 * 60);
	int seconds =
		((static_cast<int>(trunc(duration)) % (1000 * 60 * 60)) %
		(1000 * 60)) / 1000;
#endif
	if (hours > 0) {
		if (minutes < 10) {
			_return += IntToStr(hours) + ":0";
		}
		else {
			_return += IntToStr(hours) + ":";
		}
	}
	if (seconds < 10) {
		_secondsStr = "0" + IntToStr(seconds);
	}
	else {
		_secondsStr = IntToStr(seconds);
	}
	return _return + IntToStr(minutes) + ":" + _secondsStr;
}

// ---------------------------------------------------------------------------
bool TMPSong::Equals(const TMPSong &song) {
	return (Artist == song.Artist) && (Album == song.Album) &&
		(Duration == song.Duration) && (Title == song.Title);
}

// ---------------------------------------------------------------------------
TMPAlbum TMPAlbum::AllMusicAlbum() {
	TMPAlbum album;
	album.Name = "All Songs";
	album.Artist = "";
	album.Album_ID = -1;
	album.Artwork = NULL;
	return album;
}
// ---------------------------------------------------------------------------
#ifdef __APPLE__

_di_NSString MPMediaItemPropertyTitle() {
	return NSSTR("title");
}

_di_NSString MPMediaItemPropertyAlbumTitle() {
	return TNSString::Wrap(TNSString::Alloc()->initWithString
		(NSSTR("albumTitle")));
	// return NSSTR("albumTitle");
}

_di_NSString MPMediaItemPropertyArtist() {
	return NSSTR("artist");
}

_di_NSString MPMediaItemPropertyArtwork() {
	return NSSTR("artwork");
}

_di_NSString MPMediaItemPropertyPlaybackDuration() {
	return NSSTR("playbackDuration");
}

_di_NSString MPMediaItemPropertyMediaType() {
	return NSSTR("mediaType");
}

_di_NSString MPMediaItemPropertyComposer() {
	return NSSTR("composer");
}

_di_NSString MPMediaItemPropertyGenre() {
	return NSSTR("genre");
}

_di_NSString MPMediaPlaylistPropertyName() {
	return NSSTR("name");
}

_di_NSString MPMediaItemPropertyPodcastTitle() {
	return NSSTR("podcastTitle");
}
#endif
} /* namespace Utils */
} /* namespace Musicplayer */
