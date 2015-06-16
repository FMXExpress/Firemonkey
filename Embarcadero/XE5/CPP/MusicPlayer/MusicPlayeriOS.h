//---------------------------------------------------------------------------

#ifndef MusicPlayeriOSH
#define MusicPlayeriOSH

#include <FMX.Graphics.hpp>
#include <FMX.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.IOUtils.hpp>
#include <iOSapi.MediaPlayer.hpp>
#include <iOSapi.Foundation.hpp>
#include <iOSapi.UIKit.hpp>
#include <FMX.Helpers.iOS.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Actions.hpp>
#include <vector>
#include "MusicPlayerUtils.h"
//---------------------------------------------------------------------------
namespace Musicplayer
{
namespace Ios
{
// Types ---------------------------------------------------------------------
enum class TMPControllerType : unsigned char { ctApplication, ctIpod };

enum class TMPRepeatMode : unsigned char { rmDefault, rmNone, rmOne, rmAll };

enum class TMPPlaybackState : unsigned char { psStopped, psPlaying, psPaused,
	psInterrupted, psSeekingForward, psSeekingBackward };


class TMusicPlayer : public TObject
{
private:
	class TProcessThread : public System::Classes::TThread
	{
	private:
		TMusicPlayer* FMusicPlayer;
		Musicplayer::Utils::TMPSong FLastItem;
		Musicplayer::Utils::TOnProcessPlayEvent FOnProcessPlay;
		void __fastcall ProcessPlay(void);
	public:
		__fastcall TProcessThread(bool CreateSuspended, __strong TMusicPlayer* AMusicPlayer, Musicplayer::Utils::TOnProcessPlayEvent processHandler);
		__fastcall virtual ~TProcessThread(void);
		virtual void __fastcall Execute(void);
	};
protected:
	static TMusicPlayer* FInstance;
private:
	unsigned int FCurrentIndex;
	System::DynamicArray<Musicplayer::Utils::TMPSong> FPlaylist;
	System::DynamicArray<Musicplayer::Utils::TMPAlbum> FAlbums;
	Iosapi::Mediaplayer::_di_MPMusicPlayerController FMusicPlayer;
	Fmx::Graphics::TBitmap* FDefaultAlbumImage;
	Musicplayer::Utils::TOnSongChangeEvent FOnSongChange;
	Musicplayer::Utils::TOnProcessPlayEvent FOnProcessPlay;

protected:
	inline __fastcall TMusicPlayer(TMPControllerType AType);

private:
	__fastcall ~TMusicPlayer(void);
	void __fastcall DoOnSongChange(int newIndex);
	void __fastcall DoOnProcessPlay(float newPos);
	void __fastcall SetVolume(const float Value);
	void __fastcall SetTime(const float Value);
	void __fastcall SetRepeatMode(const TMPRepeatMode Value);
	void __fastcall SetShuffleMode(const bool Value);
	float __fastcall GetVolume(void);
	float __fastcall GetTime(void);
	TMPRepeatMode __fastcall GetRepeatMode(void);
	float __fastcall GetDuration(void);
	TMPPlaybackState __fastcall GetPlaybackState(void);
	bool __fastcall GetShuffleMode(void);
public:
	static void __fastcall SetPlayerType(TMPControllerType AType);
	static  TMusicPlayer* __fastcall DefaultPlayer();
	__property unsigned CurrentIndex = {read=FCurrentIndex};
	__property float Volume = {read=GetVolume, write=SetVolume};
	__property float Time = {read=GetTime, write=SetTime};
	__property float Duration = {read=GetDuration};
	__property TMPPlaybackState PlaybackState = {read=GetPlaybackState};
	__property bool ShuffleMode = {read=GetShuffleMode, write=SetShuffleMode};
	__property TMPRepeatMode RepeatMode = {read=GetRepeatMode, write=SetRepeatMode};
	__property System::DynamicArray<Musicplayer::Utils::TMPSong> Playlist = {read=FPlaylist};
	__property System::DynamicArray<Musicplayer::Utils::TMPAlbum> Albums = {read=FAlbums};
	__property Fmx::Graphics::TBitmap* DefaultAlbumImage = {read=FDefaultAlbumImage, write=FDefaultAlbumImage};
	__property Musicplayer::Utils::TOnSongChangeEvent OnSongChange = {read=FOnSongChange, write=FOnSongChange};
	__property Musicplayer::Utils::TOnProcessPlayEvent OnProcessPlay = {read=FOnProcessPlay, write=FOnProcessPlay};
public:
	NativeUInt __fastcall IndexOfNowPlayingItem(void);
	System::DynamicArray<System::UnicodeString> __fastcall GetAlbums(void);
	System::DynamicArray<System::UnicodeString> __fastcall GetSongs(void);
	System::DynamicArray<System::UnicodeString> __fastcall GetSongsInAlbum(System::UnicodeString AName);
	void __fastcall PlayByIndex(unsigned Index);
	void __fastcall Play(void);
	void __fastcall Stop(void);
	void __fastcall Pause(void);
	void __fastcall Next(void);
	void __fastcall Previous(void);
};

// template for Invoke TThreadProcedure
template <typename M, typename P>
class MyThreadMethod : public TInterfacedObject, public TThreadProcedure {
private:
	M m_Method;
	P m_param;
public:
	// constructor
	__inline __fastcall MyThreadMethod(M _m, P _p) : m_Method(_m), m_param(_p){};
	// TInterfacedObject methods
	HRESULT STDMETHODCALLTYPE QueryInterface (REFIID riid, void** ppvObject){
		return TInterfacedObject::QueryInterface (riid, ppvObject);
	}
	ULONG STDMETHODCALLTYPE AddRef() { return TInterfacedObject::_AddRef(); }
	ULONG STDMETHODCALLTYPE Release() { return TInterfacedObject::_Release(); }

	void __fastcall Invoke(void){
		m_Method(m_param);
	}
};

}	/* namespace Ios */
}	/* namespace Musicplayer */
#endif
