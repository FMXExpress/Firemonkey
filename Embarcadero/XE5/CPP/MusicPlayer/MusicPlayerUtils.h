//---------------------------------------------------------------------------

#ifndef MusicPlayerUtilsH
#define MusicPlayerUtilsH

#include <FMX.Types.hpp>
#include <iOSApi.MediaPlayer.hpp>
#include <iOSApi.Foundation.hpp>
#include <System.SysUtils.hpp>
#include <FMX.Graphics.hpp>
//---------------------------------------------------------------------------
namespace Musicplayer
{
namespace Utils
{

struct TMPSong
{
public:
	System::UnicodeString Album;
	System::UnicodeString Artist;
	System::UnicodeString Duration;
	System::UnicodeString Title;
	Iosapi::Mediaplayer::_di_MPMediaItem MPItem;
	static TMPSong __fastcall FromMediaItem(Iosapi::Mediaplayer::_di_MPMediaItem Item);
	static TMPSong __fastcall EmptySong();
	static System::UnicodeString __fastcall DurationToString(float duration);
	bool __fastcall Equals(const TMPSong &song);
};


struct TMPAlbum
{
public:
	System::UnicodeString Name;
	System::UnicodeString Artist;
	int Album_ID;
	__strong Fmx::Graphics::TBitmap* Artwork;
	static TMPAlbum __fastcall AllMusicAlbum();
};

typedef void __fastcall (__closure *TOnSongChangeEvent)(int newIndex);

typedef void __fastcall (__closure *TOnProcessPlayEvent)(float newPos);


_di_NSString __fastcall MPMediaItemPropertyTitle(void);
_di_NSString __fastcall MPMediaItemPropertyAlbumTitle(void);
_di_NSString __fastcall MPMediaItemPropertyArtist(void);
_di_NSString __fastcall MPMediaItemPropertyArtwork(void);
_di_NSString __fastcall MPMediaItemPropertyPlaybackDuration(void);
_di_NSString __fastcall MPMediaItemPropertyMediaType(void);
_di_NSString __fastcall MPMediaItemPropertyComposer(void);
_di_NSString __fastcall MPMediaItemPropertyGenre(void);
_di_NSString __fastcall MPMediaPlaylistPropertyName(void);
_di_NSString __fastcall MPMediaItemPropertyPodcastTitle(void);

}	/* namespace Utils */
}	/* namespace Musicplayer */

#endif

